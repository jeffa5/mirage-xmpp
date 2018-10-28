open Core_kernel

module type Flowout = Flowout.Main

module Main (F : Flowout) = struct
  let make_parser parser_status stream =
    Markup_lwt.parse_xml
      ~report:(fun _ e ->
        let error_string = Markup.Error.to_string e in
        Logs.warn (fun f -> f "Error occurred during parsing: %s" error_string);
        match e with
        | `Bad_document _ ->
          parser_status := Error error_string;
          Lwt.return_unit
        | `Unmatched_end_tag _ | `Unmatched_start_tag _ ->
          parser_status := Error error_string;
          Lwt.return_unit
        | _ -> Lwt.return_unit )
      stream
  ;;

  let pull_signal flow signals parser_status =
    let rec aux depth =
      let%lwt signal = Markup_lwt.next signals in
      match !parser_status with
      | Error s ->
        Logs.warn (fun f ->
            f "Parser report generated an error. Notifying user and exiting parsing." );
        F.write_error_string flow s
      | Ok _ ->
        (match signal with
        | Some signal ->
          (match signal with
          | `Start_element _ ->
            Logs.debug (fun f ->
                f "Start element received: %s" (Markup.signal_to_string signal) );
            aux (depth + 1)
          | `End_element ->
            Logs.debug (fun f -> f "End element received");
            if depth = 1
            then (
              Logs.info (fun f -> f "Accepting the parsed XML and notifying user");
              F.write_string flow "XML accepted." )
            else aux (depth - 1)
          | `Text _ ->
            let signal_string = String.strip (Markup.signal_to_string signal) in
            if signal_string <> ""
            then Logs.debug (fun f -> f "Text received: %s" signal_string);
            aux depth
          | _ ->
            Logs.debug (fun f -> f "Signal received! %s" (Markup.signal_to_string signal));
            aux depth)
        | None ->
          Logs.debug (fun f -> f "None signal received");
          Lwt.return_unit)
    in
    aux 0
  ;;

  let parse_xml flow stream =
    let parser_status = ref (Ok ()) in
    let signals =
      Markup_lwt.lwt_stream stream |> make_parser parser_status |> Markup.signals
    in
    Logs.info (fun f -> f "Setup parser, beginning to pull signals.");
    pull_signal flow signals parser_status
  ;;
end
