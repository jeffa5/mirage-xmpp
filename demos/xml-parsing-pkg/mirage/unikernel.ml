open Core_kernel

module Main (S : Mirage_stack_lwt.V4) = struct
  let write_string flow s =
    let s = String.strip s ^ "\n" in
    let b = Cstruct.of_string s in
    match%lwt S.TCPV4.write flow b with
    | Ok () -> Lwt.return_unit
    | Error e ->
      Logs.warn (fun f ->
          f "Error occurred from writing to connection: %a" S.TCPV4.pp_write_error e );
      Lwt.return_unit
  ;;

  let write_error_string flow s =
    let s = "Error: " ^ String.strip s in
    write_string flow s
  ;;

  let read flow pushf =
    let rec aux () =
      match%lwt S.TCPV4.read flow with
      | Ok `Eof | Error _ -> Lwt.return_unit
      | Ok (`Data b) ->
        let s = Cstruct.to_string b in
        Logs.info (fun f -> f "Data read from connection: %s" s);
        String.iter ~f:(fun c -> pushf (Some c)) s;
        aux ()
    in
    aux ()
  ;;

  let on_connect flow =
    let stream, pushf = Lwt_stream.create () in
    Lwt.async (fun () -> read flow pushf);
    let%lwt () =
      match%lwt Xmlparser.parse_xml stream with
      | Ok (Some s) -> write_string flow s
      | Ok None -> Lwt.return_unit
      | Error s -> write_error_string flow s
    in
    Logs.info (fun f -> f "Closing the connection");
    S.TCPV4.close flow
  ;;

  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port on_connect;
    S.listen s
  ;;
end
