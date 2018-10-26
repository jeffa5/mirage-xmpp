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
        String.iter ~f:(fun c -> pushf (Some c)) s;
        aux ()
    in
    aux ()
  ;;

  let parse_xml flow stream =
    let stream = Markup_lwt.lwt_stream stream in
    let parser_report = ref (Ok ()) in
    let parser =
      Markup_lwt.parse_xml
        ~report:(fun _ e ->
          let error_string = Markup.Error.to_string e in
          Logs.warn (fun f -> f "Error occurred during parsing: %s" error_string);
          match e with
          | `Bad_document _ ->
            parser_report := Error error_string;
            Lwt.return_unit
          | `Unmatched_end_tag _ | `Unmatched_start_tag _ ->
            parser_report := Error error_string;
            Lwt.return_unit
          | _ -> Lwt.return_unit )
        stream
    in
    let signals = Markup.signals parser in
    Logs.info (fun f -> f "Setup parser and signals, beginning to pull signals.");
    let rec pull_signal depth =
      let%lwt signal = Markup_lwt.next signals in
      match !parser_report with
      | Error s ->
        Logs.warn (fun f ->
            f "Parser report generated an error. Notifying user and exiting parsing." );
        write_error_string flow s
      | Ok _ ->
        (match signal with
        | Some signal ->
          (match signal with
          | `Start_element _ ->
            Logs.debug (fun f ->
                f "Start element received: %s" (Markup.signal_to_string signal) );
            pull_signal (depth + 1)
          | `End_element ->
            Logs.debug (fun f -> f "End element received");
            if depth = 1
            then (
              Logs.info (fun f -> f "Accepting the parsed XML and notifying user");
              write_string flow "XML accepted." )
            else pull_signal (depth - 1)
          | `Text _ ->
            let signal_string = String.strip (Markup.signal_to_string signal) in
            if signal_string <> ""
            then Logs.debug (fun f -> f "Text received: %s" signal_string);
            pull_signal depth
          | _ ->
            Logs.debug (fun f -> f "Signal received! %s" (Markup.signal_to_string signal));
            pull_signal depth)
        | None ->
          Logs.debug (fun f -> f "None signal received");
          Lwt.return_unit)
    in
    pull_signal 0
  ;;

  let on_connect flow =
    let stream, pushf = Lwt_stream.create () in
    Lwt.async (fun () -> read flow pushf);
    let%lwt () = parse_xml flow stream in
    Logs.info (fun f -> f "Closing the connection");
    S.TCPV4.close flow
  ;;

  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port on_connect;
    S.listen s
  ;;
end
