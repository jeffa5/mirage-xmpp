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
        let signal_string = String.trim (Markup.signal_to_string signal) in
        if signal_string <> ""
        then Logs.debug (fun f -> f "Text received: %s" signal_string);
        aux depth
      | _ ->
        Logs.debug (fun f -> f "Signal received! %s" (Markup.signal_to_string signal));
        aux depth)
    | None ->
      Logs.debug (fun f -> f "No signal received, stream empty");
      Lwt.return_ok None
  in
  aux 0
;;

let parse_xml stream =
  let signals = Markup_lwt.lwt_stream stream |> make_parser |> Markup.signals in
  Logs.info (fun f -> f "Setup parser, beginning to pull signals.");
  pull_signal signals
;;

(* Set the logging up for the unit tests. Use the default source and formatter and debug to show all logs *)
let setup_logs () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  ()
;;

let parse_string str =
  setup_logs ();
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
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [DEBUG] No signal received, stream empty |}]
;;

let%expect_test "no start tag" =
  parse_string "</error>";
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [WARNING] Error occurred during parsing: bad document: expected root element
    bad document: expected root element |}]
;;

let%expect_test "unmatched end tag" =
  parse_string "<start></a></start>";
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [DEBUG] Start element received: <start>
    run.exe: [WARNING] Error occurred during parsing: unmatched end tag 'a'
    unmatched end tag 'a' |}]
;;

let%expect_test "unmatched start tag" =
  parse_string "<start><a></start>";
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [DEBUG] Start element received: <start>
    run.exe: [DEBUG] Start element received: <a>
    run.exe: [WARNING] Error occurred during parsing: unmatched start tag 'a'
    unmatched start tag 'a' |}]
;;

let%expect_test "simple open close" =
  parse_string "<stream></stream>";
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [DEBUG] Start element received: <stream>
    run.exe: [DEBUG] End element received
    run.exe: [INFO] Accepting the parsed XML and notifying user
    XML accepted. |}]
;;

let%expect_test "no closing tag" =
  parse_string "<message><body>No closing tag!</message>";
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [DEBUG] Start element received: <message>
    run.exe: [DEBUG] Start element received: <body>
    run.exe: [DEBUG] Text received: No closing tag!
    run.exe: [WARNING] Error occurred during parsing: unmatched start tag 'body'
    unmatched start tag 'body' |}]
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
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [DEBUG] Start element received: <http://etherx.jabber.org/streams:stream from="juliet@im.example.com" to="im.example.com" version="1.0" http://www.w3.org/XML/1998/namespace:lang="en" http://www.w3.org/2000/xmlns/:xmlns="jabber:client" http://www.w3.org/2000/xmlns/:stream="http://etherx.jabber.org/streams">
    run.exe: [DEBUG] Start element received: <jabber:client:message>
    run.exe: [DEBUG] Start element received: <jabber:client:body>
    run.exe: [DEBUG] Text received: foo
    run.exe: [DEBUG] End element received
    run.exe: [DEBUG] End element received
    run.exe: [DEBUG] End element received
    run.exe: [INFO] Accepting the parsed XML and notifying user
    XML accepted. |}]
;;

let%expect_test "unknown namespace" =
  parse_string
    "<stream:error>\n\
    \        <not-well-formed\n\
    \            xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>\n\
    \      </stream:error>\n\
    \      </stream:stream>";
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [WARNING] Error occurred during parsing: unknown namespace 'stream'
    unknown namespace 'stream' |}]
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
  [%expect
    {|
    run.exe: [INFO] Setup parser, beginning to pull signals.
    run.exe: [DEBUG] Signal received! <?xml version="1.0">?>
    run.exe: [DEBUG] Start element received: <http://etherx.jabber.org/streams:stream from="im.example.com" id="++TR84Sm6A3hnt3Q065SnAbbk3Y=" to="juliet@im.example.com" version="1.0" http://www.w3.org/XML/1998/namespace:lang="en" http://www.w3.org/2000/xmlns/:xmlns="jabber:client" http://www.w3.org/2000/xmlns/:stream="http://etherx.jabber.org/streams">
    run.exe: [DEBUG] Start element received: <http://etherx.jabber.org/streams:error>
    run.exe: [DEBUG] Start element received: <urn:ietf:params:xml:ns:xmpp-streams:invalid-namespace http://www.w3.org/2000/xmlns/:xmlns="urn:ietf:params:xml:ns:xmpp-streams">
    run.exe: [DEBUG] End element received
    run.exe: [DEBUG] End element received
    run.exe: [DEBUG] End element received
    run.exe: [INFO] Accepting the parsed XML and notifying user
    XML accepted. |}]
;;
