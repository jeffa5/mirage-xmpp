open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct

  let write_string flow s =
    S.TCPV4.write flow s >>= function
    | Ok () -> Lwt.return_unit
    | Error e -> Logs.warn (fun f -> f "Error occurred from writing to connection: %a" S.TCPV4.pp_write_error e); Lwt.return_unit

  let attr_to_string = function
    | ((uri, local), value) -> uri ^ ":" ^ local ^ "=" ^ value

  let parse_xml i =
    (* Need to catch errors thrown when invalid xml is received *)
    let rec pull i depth =
      match Xmlm.input i with
      | `El_start ((uri, local), attr_list) ->
        Logs.debug (fun f -> f "Found the start of an element: %s:%s" uri local);
        List.iter (fun attr -> Logs.debug (fun f -> f "attribute: %s" (attr_to_string attr))) attr_list;
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
    in pull i 0 >>= fun () ->
    if not (Xmlm.eoi i) then Logs.warn (fun f -> f "Invalid xml received");
    Lwt.return_unit

  let xml_source flow =
    let index = ref 0 in
    let bytes = ref None in
    let rec read_byte () =
      match !bytes with
      | None -> (
          match Lwt_main.run (S.TCPV4.read flow) with
          | Ok `Eof -> raise End_of_file
          | Error _ -> raise End_of_file
          | Ok (`Data b) ->
            index := 0;
            bytes := Some (Cstruct.to_bytes b);
            read_byte ()
        )
      | Some bs ->
        if !index = Bytes.length bs then (
          index := 0;
          bytes := None;
          read_byte ()
        )
        else (
          let byte = Bytes.get bs !index in
          index := !index + 1;
          Char.code byte
        )
    in
    read_byte

  let rec read_xml flow =
    S.TCPV4.read flow >>= function
    | Ok `Eof -> Logs.info (fun f -> f "Closing connection due to Eof!"); Lwt.return_unit
    | Error e ->
      Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e);
      Lwt.return_unit
    | Ok (`Data b) ->
      Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));
      (* Need to create a channel which we can use to write the given string into *)
      let i = Xmlm.make_input (`Fun (xml_source flow)) in
      (* let i = Xmlm.make_input (`String (0, Cstruct.to_string b)) in *)
      parse_xml i >>= fun () ->
      read_xml flow

  let on_connect flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
    read_xml flow >>= fun () ->
    S.TCPV4.close flow

  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port on_connect;
    S.listen s

end
