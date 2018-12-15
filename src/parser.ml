type t =
  { stream : (Markup.signal, Markup.async) Markup.stream
  ; mutable depth : int }

type parse_result =
  | Stanza of Stanza.t
  | Stream_Element of Stream.t
  | Error of string

exception ParsingError of string

let make_parser stream =
  Markup_lwt.parse_xml
    ~report:(fun _ e ->
      let error_string = Markup.Error.to_string e in
      Lwt.fail (ParsingError error_string) )
    stream
;;

let create stream =
  let stream = Markup_lwt.lwt_stream stream |> make_parser |> Markup.signals in
  {stream; depth = 0}
;;

let rec parse_children parser =
  match%lwt Markup_lwt.next parser.stream with
  | exception ParsingError e -> Lwt.return_error e
  | Some signal ->
    (match signal with
    | `Start_element tag ->
      (match%lwt parse_children parser with
      | Ok children ->
        let element = Xml.Element (tag, children) in
        (match%lwt parse_children parser with
        | Ok element_list -> Lwt.return_ok (element :: element_list)
        | Error e -> Lwt.return_error e)
      | Error e -> Lwt.return_error e)
    | `End_element -> Lwt.return_ok []
    | `Text ss ->
      let text = Xml.Text (String.concat "\n" ss) in
      (match%lwt parse_children parser with
      | Ok element_list -> Lwt.return_ok (text :: element_list)
      | Error e -> Lwt.return_error e)
    | _ -> assert false)
  | None -> Lwt.return_error "End of parsing stream"
;;

let parse parser =
  match%lwt Markup_lwt.next parser.stream with
  | exception ParsingError e -> Lwt.return (Error e)
  | Some signal ->
    (match signal with
    | `Start_element (((_namespace, name), _attrs) as tag) ->
      (match parser.depth with
      | 0 ->
        (* start of stream *)
        parser.depth <- parser.depth + 1;
        Lwt.return (Stream_Element (Stream.Header tag))
      | 1 ->
        (* parse stanza / error / feature *)
        (match name with
        | "iq" ->
          (match%lwt parse_children parser with
          | Ok children -> Lwt.return (Stanza (Stanza.Iq (Xml.Element (tag, children))))
          | Error e -> Lwt.return (Error e))
        | "message" ->
          (match%lwt parse_children parser with
          | Ok children ->
            Lwt.return (Stanza (Stanza.Message (Xml.Element (tag, children))))
          | Error e -> Lwt.return (Error e))
        | "presence" ->
          (match%lwt parse_children parser with
          | Ok children ->
            Lwt.return (Stanza (Stanza.Presence (Xml.Element (tag, children))))
          | Error e -> Lwt.return (Error e))
        | s -> Lwt.return (Error ("Unexpected tag with name: " ^ s)))
      | _ -> assert false)
    | `End_element ->
      (match parser.depth with
      | 1 -> (* End of the stream *)
             Lwt.return (Stream_Element Stream.Close)
      | _ -> assert false)
    | _ -> Lwt.return (Error "base match case not implemented"))
  | None -> Lwt.return (Error "Closed parser stream")
;;

let parse_string s =
  let parser = create (Lwt_stream.of_string s) in
  let out () =
    match%lwt parse parser with
    | Stanza s ->
      print_endline (Stanza.to_string s);
      Lwt.return_unit
    | Stream_Element stream_element ->
      print_endline ("Stream_Element\n" ^ Stream.to_string stream_element);
      Lwt.return_unit
    | Error e ->
      print_endline e;
      Lwt.return_unit
  in
  fun () -> Lwt_main.run (out ())
;;

let%expect_test "initial stanza gets returned" =
  let pf = parse_string "<stream></stream>" in
  pf ();
  [%expect {|
    Stream_Element
    <stream> |}];
  pf ();
  [%expect {|
    Stream_Element
    </stream:stream> |}]
;;

let%expect_test "empty stanza should be an error" =
  let pf = parse_string "<stream><message/></stream>" in
  pf ();
  [%expect {|
    Stream_Element
    <stream> |}];
  pf ();
  [%expect {| <message/> |}]
;;

let%expect_test "non empty stanza is ok" =
  let pf = parse_string "<stream><message><body>A message!</body></message></stream>" in
  pf ();
  [%expect {|
    Stream_Element
    <stream> |}];
  pf ();
  [%expect {|
    <message><body>A message!</body></message> |}];
  pf ();
  [%expect {|
    Stream_Element
    </stream:stream> |}]
;;

let%expect_test "start end full" =
  let pf =
    parse_string
      "<stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' \
       xml:lang='en' xmlns='jabber:client' \
       xmlns:stream='http://etherx.jabber.org/streams'><message><body>text</body></message></stream:stream>"
  in
  pf ();
  [%expect
    {|
    Stream_Element
    <http://etherx.jabber.org/streams:stream from='juliet@im.example.com' to='im.example.com' version='1.0' http://www.w3.org/XML/1998/namespace:lang='en' http://www.w3.org/2000/xmlns/:xmlns='jabber:client' http://www.w3.org/2000/xmlns/:stream='http://etherx.jabber.org/streams'> |}];
  pf ();
  [%expect
    {|
    <jabber:client:message><jabber:client:body>text</jabber:client:body></jabber:client:message> |}];
  pf ();
  [%expect {|
    Stream_Element
    </stream:stream> |}];
  pf ();
  [%expect {| Closed parser stream |}]
;;

let%expect_test "resource binding" =
  let pf =
    parse_string
      "<stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' \
       xml:lang='en' xmlns='jabber:client' \
       xmlns:stream='http://etherx.jabber.org/streams'><iq id='yhc13a95' \
       type='set'><bind \
       xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>balcony</resource></bind></iq></stream:stream>"
  in
  pf ();
  [%expect
    {|
    Stream_Element
    <http://etherx.jabber.org/streams:stream from='juliet@im.example.com' to='im.example.com' version='1.0' http://www.w3.org/XML/1998/namespace:lang='en' http://www.w3.org/2000/xmlns/:xmlns='jabber:client' http://www.w3.org/2000/xmlns/:stream='http://etherx.jabber.org/streams'> |}];
  pf ();
  [%expect
    {|
    <jabber:client:iq id='yhc13a95' type='set'><urn:ietf:params:xml:ns:xmpp-bind:bind http://www.w3.org/2000/xmlns/:xmlns='urn:ietf:params:xml:ns:xmpp-bind'><urn:ietf:params:xml:ns:xmpp-bind:resource>balcony</urn:ietf:params:xml:ns:xmpp-bind:resource></urn:ietf:params:xml:ns:xmpp-bind:bind></jabber:client:iq> |}];
  pf ();
  [%expect {|
      Stream_Element
      </stream:stream> |}]
;;
