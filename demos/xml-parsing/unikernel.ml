open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct

  let write_string flow s =
    S.TCPV4.write flow s >>= function
    | Ok () -> Lwt.return_unit
    | Error e -> Logs.warn (fun f -> f "Error occurred from writing to connection: %a" S.TCPV4.pp_write_error e); Lwt.return_unit

  let parse_xml i =
    (* Need to catch errors thrown when invalid xml is received *)
    let rec pull i depth =
      match Xmlm.input i with
      | `El_start ((_, local), _) ->
        Logs.debug (fun f -> f "Found the start of an element with tag: %s" local);
        pull i (depth + 1)
      | `El_end -> Logs.debug (fun f -> f "Found an end of an element at depth %d" depth);
        if depth = 1 then Lwt.return_unit else pull i (depth - 1)
      | `Data s ->
        Logs.debug (fun f -> f "Received the data: %s" s);
        pull i depth
      | `Dtd opt -> match opt with
        | Some s -> Logs.debug (fun f -> f "Dtd found: %s" s);
          pull i depth
        | None -> Logs.debug (fun f -> f "Skipping Dtd");
          pull i depth
    in pull i 0

  let rec read_and_respond flow =
    S.TCPV4.read flow >>= function
    | Ok `Eof -> Logs.info (fun f -> f "Closing connection due to Eof!"); Lwt.return_unit
    | Error e ->
      Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e);
      Lwt.return_unit
    | Ok (`Data b) ->
      Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));
      (* Need to create a channel which we can use to write the given string into *)
      let i = Xmlm.make_input (`String (0, Cstruct.to_string b)) in
      parse_xml i >>= fun () ->
      read_and_respond flow

  let on_connect flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
    read_and_respond flow >>= fun () ->
    S.TCPV4.close flow

  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port on_connect;
    S.listen s

end
