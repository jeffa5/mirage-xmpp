open Lwt.Infix


module Main (S: Mirage_stack_lwt.V4) = struct

  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (fun flow ->
        let dst, dst_port = S.TCPV4.dst flow in
        (* Log the information of the incoming connection *)
        Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                      (Ipaddr.V4.to_string dst) dst_port);
        S.TCPV4.read flow >>= function
        | Ok `Eof -> Logs.info (fun f -> f "Closing connection!"); Lwt.return_unit
        | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
        | Ok (`Data b) ->
          Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));
          let message = Cstruct.of_string "*\n* Hello from Mirage!\n*\n" in
          S.TCPV4.write flow message >>= function
          | Ok () -> S.TCPV4.close flow
          | Error e -> Logs.warn (fun f -> f "Error occurred from writing to connection: %a" S.TCPV4.pp_write_error e); Lwt.return_unit
      );

    S.listen s

end
