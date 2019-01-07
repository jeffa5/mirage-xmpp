type t =
  { raw_stream : char Lwt_stream.t
  ; stream : (Markup.signal, Markup.async) Markup.stream
  ; mutable depth : int }

type parse_result =
  | Stanza of Stanza.t
  | Sasl_auth of Xml.t
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

let create raw_stream =
  let stream = Markup_lwt.lwt_stream raw_stream |> make_parser |> Markup.signals in
  {raw_stream; stream; depth = 0}
;;

let reset parser =
  { raw_stream = parser.raw_stream
  ; stream = Markup_lwt.lwt_stream parser.raw_stream |> make_parser |> Markup.signals
  ; depth = 0 }
;;

let convert_attribute ((namespace, name), value) =
  let open Xml in
  ( namespace
  , match name with
    | "from" -> From (Jid.of_string value)
    | "to" -> To (Jid.of_string value)
    | "id" -> Id value
    | "jid" -> Jid (Jid.of_string value)
    | "xmlns" -> Xmlns value
    | "type" -> Type value
    | "ver" -> Ver value
    | "version" -> Version value
    | "lang" -> Lang value
    | "stream" -> Stream value
    | "name" -> Name value
    | "subscription" -> Subscription (Rosters.subscription_of_string value)
    | "mechanism" -> Mechanism value
    | _ -> Other (name, value) )
;;

let convert_attributes attributes =
  List.map (fun attr -> convert_attribute attr) attributes
;;

let rec parse_children parser =
  match%lwt Markup_lwt.next parser.stream with
  | exception ParsingError e -> Lwt.return_error e
  | Some signal ->
    (match signal with
    | `Start_element (name, attributes) ->
      let tag = name, convert_attributes attributes in
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

let rec parse parser =
  match%lwt Markup_lwt.next parser.stream with
  | exception ParsingError e -> Lwt.return (Error e)
  | Some signal ->
    (match signal with
    | `Start_element ((namespace, name), attrs) ->
      let tag = (namespace, name), convert_attributes attrs in
      (match parser.depth with
      | 0 ->
        (* start of stream *)
        (* check it actually is a stream tag *)
        if name = "stream"
        then (
          parser.depth <- 1;
          Lwt.return (Stream_Element (Stream.Header tag)) )
        else
          Lwt.return
            (Error
               ("Invalid initial stanza with name " ^ name ^ ", expected stream header."))
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
        | "auth" ->
          (match%lwt parse_children parser with
          | Ok children -> Lwt.return (Sasl_auth (Xml.Element (tag, children)))
          | Error e -> Lwt.return (Error e))
        | "stream" ->
          parser.depth <- 1;
          Lwt.return (Stream_Element (Stream.Header tag))
        | s -> Lwt.return (Error ("Unexpected tag with name: " ^ s)))
      | _ -> assert false)
    | `End_element ->
      (match parser.depth with
      | 1 -> (* End of the stream *)
             Lwt.return (Stream_Element Stream.Close)
      | _ -> Lwt.return (Error "Unexpected end element in parser"))
    | `Text ss ->
      (match String.trim (String.concat "" ss) with
      | "" -> parse parser
      | _ -> Lwt.return (Error ("Unexpected Text: " ^ String.concat "\n" ss)))
    | `Xml _declaration ->
      (* Xml declaration is optional so we can just ignore it as there is nothing to do with it *)
      parse parser
    | `Doctype _doctype -> Lwt.return (Error "Unexpected Doctype")
    | `PI (s1, s2) -> Lwt.return (Error ("Unexpected PI: " ^ s1 ^ ", " ^ s2))
    | `Comment s -> Lwt.return (Error ("Unexpected Comment: " ^ s)))
  | None -> Lwt.return (Error "End of parsing stream")
;;

let parse_string s =
  let parser = create (Lwt_stream.of_string s) in
  let out () =
    match%lwt parse parser with
    | Stanza s ->
      print_endline (Stanza.to_string s);
      Lwt.return_unit
    | Sasl_auth xml ->
      print_endline ("Sasl_auth\n" ^ Xml.to_string xml);
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
  [%expect {| End of parsing stream |}]
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

let%expect_test "invalid xml" =
  let pf = parse_string "<stream><iq></stream>" in
  pf ();
  [%expect {|
      Stream_Element
      <stream> |}];
  pf ();
  [%expect {| unmatched start tag 'iq' |}]
;;

let%expect_test "whitespace between elements" =
  let pf = parse_string "<stream>  <iq>\n</iq> </stream>" in
  pf ();
  [%expect {|
      Stream_Element
      <stream> |}];
  pf ();
  [%expect {|
    <iq>
    </iq> |}]
;;

let%expect_test "non-whitespace between elements" =
  let pf =
    parse_string "<stream>invalid string <iq> n </iq>more invalid stuff</stream>"
  in
  pf ();
  [%expect {|
      Stream_Element
      <stream> |}];
  pf ();
  [%expect {|
    Unexpected Text: invalid string |}]
;;
