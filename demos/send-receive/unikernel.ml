open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct

  let write_string flow s =
    S.TCPV4.write flow s >>= function
    | Ok () -> Lwt.return_unit
    | Error e -> Logs.warn (fun f -> f "Error occurred from writing to connection: %a" S.TCPV4.pp_write_error e); Lwt.return_unit

  let rec echo flow =
    S.TCPV4.read flow >>= function
    | Ok `Eof -> Logs.info (fun f -> f "Closing connection due to Eof!"); Lwt.return_unit
    | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
    | Ok (`Data b) ->
      Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));
      let reply = (Cstruct.of_string ("echo: " ^ Cstruct.to_string b)) in
      write_string flow reply >>= fun () ->
      echo flow

  let on_connect flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
    let welcome = Cstruct.of_string "*\n* Hello from Mirage!\n*\n" in
    write_string flow welcome >>= fun () ->
    echo flow >>= fun () ->
    S.TCPV4.close flow

  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port on_connect;
    S.listen s

end
