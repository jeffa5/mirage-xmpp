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

let parse_string str =
  let stream = Lwt_stream.of_string str in
  let out =
    match%lwt parse_xml stream with
    | Ok (Some s) -> Lwt.return s
    | Ok None -> Lwt.return ""
    | Error e -> Lwt.return e
  in
  let s = Lwt_main.run out in
  print_endline s
;;

let%expect_test "empty string" =
  parse_string "";
  [%expect {| |}]
;;

let%expect_test "no start tag" =
  parse_string "</error>";
  [%expect {| bad document: expected root element |}]
;;

let%expect_test "unmatched end tag" =
  parse_string "<start></a></start>";
  [%expect {| unmatched end tag 'a' |}]
;;

let%expect_test "unmatched start tag" =
  parse_string "<start><a></start>";
  [%expect {| unmatched start tag 'a' |}]
;;

let%expect_test "simple open close" =
  parse_string "<stream></stream>";
  [%expect {| XML accepted. |}]
;;

let%expect_test "no closing tag" =
  parse_string "<message><body>No closing tag!</message>";
  [%expect {| unmatched start tag 'body' |}]
;;

let%expect_test "xmpp initial" =
  parse_string
    "<stream:stream\n\
    \       from='juliet@im.example.com'\n\
    \       to='im.example.com'\n\
    \       version='1.0'\n\
    \       xml:lang='en'\n\
    \       xmlns='jabber:client'\n\
    \       xmlns:stream='http://etherx.jabber.org/streams'>\n\
    \     <message>\n\
    \       <body>foo</body>\n\
    \     </message>\n\
    \   </stream:stream>";
  [%expect {| XML accepted. |}]
;;

let%expect_test "unknown namespace" =
  parse_string
    "<stream:error>\n\
    \        <not-well-formed\n\
    \            xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>\n\
    \      </stream:error>\n\
    \      </stream:stream>";
  [%expect {| unknown namespace 'stream' |}]
;;

let%expect_test "xmpp initial extended" =
  parse_string
    "<?xml version='1.0'?>\n\
    \      <stream:stream\n\
    \          from='im.example.com'\n\
    \          id='++TR84Sm6A3hnt3Q065SnAbbk3Y='\n\
    \          to='juliet@im.example.com'\n\
    \          version='1.0'\n\
    \          xml:lang='en'\n\
    \          xmlns='jabber:client'\n\
    \          xmlns:stream='http://etherx.jabber.org/streams'>\n\
    \      <stream:error>\n\
    \        <invalid-namespace\n\
    \            xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>\n\
    \      </stream:error>\n\
    \      </stream:stream>";
  [%expect {| XML accepted. |}]
;;
