open Core_kernel

exception ParsingError of string

let make_parser stream =
  Markup_lwt.parse_xml
    ~report:(fun _ e ->
      let error_string = Markup.Error.to_string e in
      Logs.warn (fun f -> f "Error occurred during parsing: %s" error_string);
      Lwt.fail (ParsingError error_string) )
    stream
;;

let pull_signal signals =
  let rec aux depth =
    match%lwt Markup_lwt.next signals with
    | exception ParsingError e -> Lwt.return_error e
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
          Lwt.return_ok (Some "XML accepted.") )
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
      Lwt.return_ok None
  in
  aux 0
;;

let parse_xml stream =
  let signals = Markup_lwt.lwt_stream stream |> make_parser |> Markup.signals in
  Logs.info (fun f -> f "Setup parser, beginning to pull signals.");
  pull_signal signals
;;
