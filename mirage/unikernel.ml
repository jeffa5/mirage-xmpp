module Main (S : Mirage_stack_lwt.V4) = struct
  let write_string flow s =
    let s = String.trim s ^ "\n" in
    let b = Cstruct.of_string s in
    match%lwt S.TCPV4.write flow b with
    | Ok () -> Lwt.return_unit
    | Error e ->
      Logs.warn (fun f ->
          f "Error occurred from writing to connection: %a" S.TCPV4.pp_write_error e );
      Lwt.return_unit
  ;;

  let write_error_string flow s =
    let s = "Error: " ^ String.trim s in
    write_string flow s
  ;;

  let read flow pushf =
    let rec aux () =
      match%lwt S.TCPV4.read flow with
      | Ok `Eof | Error _ -> Lwt.return_unit
      | Ok (`Data b) ->
        let s = Cstruct.to_string b in
        Logs.info (fun f -> f "Data read from connection:\n%s" s);
        String.iter (fun c -> pushf (Some c)) s;
        aux ()
    in
    aux ()
  ;;

  let write flow out_stream =
    let rec aux () =
      match%lwt Lwt_stream.get out_stream with
      | Some s ->
        Logs.debug (fun f -> f "Sending string: %s" s);
        let%lwt () = write_string flow s in
        aux ()
      | None ->
        Logs.info (fun f -> f "Out stream closed");
        Lwt.return_unit
    in
    aux ()
  ;;

  let on_connect connections flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f ->
        f "New tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port
    );
    let stream, pushf = Lwt_stream.create () in
    Lwt.async (fun () -> read flow pushf);
    let outstream, outfun = Lwt_stream.create () in
    Lwt.async (fun () -> write flow outstream);
    let roster = Mirage_xmpp.Roster.empty in
    let handler =
      Mirage_xmpp.Handler.create ~connections ~roster ~stream ~callback:outfun
    in
    let%lwt () = Mirage_xmpp.Handler.handle handler in
    Logs.info (fun f -> f "Closing the connection");
    S.TCPV4.close flow
  ;;

  let start s =
    Logs.info (fun f -> f "Started Unikernel");
    let port = Key_gen.port () in
    Logs.info (fun f -> f "Port is: %d" port);
    let connections = ref Mirage_xmpp.Connections.empty in
    S.listen_tcpv4 s ~port (on_connect connections);
    S.listen_tcpv4 s ~port:(port + 1) (fun flow ->
        Logs.info (fun f -> f "Received exit signal");
        exit 0 );
    Logs.info (fun f -> f "Started listening");
    S.listen s
  ;;
end
