open Core_kernel

module type Flowout = Flowout.Main

module Main (F : Flowout) = struct
  exception ParsingError

  let make_parser flow stream =
    Markup_lwt.parse_xml
      ~report:(fun _ e ->
        let error_string = Markup.Error.to_string e in
        Logs.warn (fun f -> f "Error occurred during parsing: %s" error_string);
        let%lwt () = F.write_error_string flow error_string in
        raise ParsingError )
      stream
  ;;

  let pull_signal flow signals =
    let rec aux depth =
      try%lwt
            match%lwt Markup_lwt.next signals with
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
                Logs.debug (fun f ->
                    f "Signal received! %s" (Markup.signal_to_string signal) );
                aux depth)
            | None ->
              Logs.debug (fun f -> f "None signal received");
              Lwt.return_unit
      with ParsingError -> Lwt.return_unit
    in
    aux 0
  ;;

  let parse_xml flow stream =
    let signals = Markup_lwt.lwt_stream stream |> make_parser flow |> Markup.signals in
    Logs.info (fun f -> f "Setup parser, beginning to pull signals.");
    pull_signal flow signals
  ;;
end
