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

  let read flow pushf =
    let dst, dst_port = S.TCPV4.dst flow in
    let dst = Ipaddr.V4.to_string dst in
    let rec aux () =
      match%lwt S.TCPV4.read flow with
      | Ok `Eof | Error _ -> Lwt.return_unit
      | Ok (`Data b) ->
        let s = Cstruct.to_string b in
        Logs.debug (fun f -> f "Read <- %s:%d : %s" dst dst_port s);
        String.iter (fun c -> pushf (Some c)) s;
        aux ()
    in
    aux ()
  ;;

  let mvar = Lwt_mvar.create_empty ()

  let write flow out_stream =
    let dst, dst_port = S.TCPV4.dst flow in
    let dst = Ipaddr.V4.to_string dst in
    let rec aux () =
      match%lwt Lwt_stream.get out_stream with
      | Some s ->
        Logs.debug (fun f -> f "Send -> %s:%d : %s" dst dst_port s);
        let%lwt () = write_string flow s in
        aux ()
      | None ->
        let%lwt () = Lwt_mvar.put mvar true in
        Lwt.return_unit
    in
    aux ()
  ;;

  let on_connect hostname flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f ->
        f "New tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port
    );
    let instream, infun = Lwt_stream.create () in
    Lwt.async (fun () -> read flow infun);
    let outstream, outfun = Lwt_stream.create () in
    Lwt.async (fun () -> write flow outstream);
    let handler =
      Mirage_xmpp.Handler.create ~stream:instream ~callback:outfun ~hostname
    in
    let%lwt () = Mirage_xmpp.Handler.handle handler in
    let%lwt _ = Lwt_mvar.take mvar in
    Logs.info (fun f ->
        f
          "Closing tcp connection from IP %s on port %d"
          (Ipaddr.V4.to_string dst)
          dst_port );
    S.TCPV4.close flow
  ;;

  let start stack =
    Logs.info (fun f -> f "Started Unikernel");
    let port = Key_gen.port () in
    let hostname = Key_gen.hostname () in
    Logs.info (fun f -> f "Port is: %d" port);
    Logs.info (fun f -> f "Hostname is: %s" hostname);
    S.listen_tcpv4 stack ~port (on_connect hostname);
    S.listen_tcpv4 stack ~port:8081 (fun _flow ->
        Logs.info (fun f -> f "Received exit signal");
        exit 0 );
    Logs.info (fun f -> f "Started listening");
    S.listen stack
  ;;
end
