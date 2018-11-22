type t =
  { stream : (Markup.signal, Markup.async) Markup.stream
  ; mutable depth : int }

type parse_result =
  | Stanza of Stanza.t
  | End
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

let parse_stanza parser =
  let rec aux () =
    let stanza = ref None in
    match%lwt Markup_lwt.next parser.stream with
    | exception ParsingError e -> Lwt.return_error e
    | Some signal ->
      (match signal with
      | `Start_element tag ->
        stanza := Some (Stanza.create tag);
        parser.depth <- parser.depth + 1;
        if parser.depth = 1
        then (* return initial stream stanza *)
          Lwt.return_ok !stanza
        else
          (* Parse all the children into this stanza *)
          let rec parse_child () =
            match%lwt aux () with
            | Ok (Some child) ->
              (match !stanza with
              | Some s ->
                stanza := Some (Stanza.add_content s child);
                parse_child ()
              | None -> assert false)
            | Ok None -> Lwt.return_ok !stanza
            | Error e -> Lwt.return_error e
          in
          parse_child ()
      | `End_element ->
        parser.depth <- parser.depth - 1;
        Lwt.return_ok !stanza
      | `Text s -> Lwt.return_ok (Some (Stanza.text s))
      | _ -> assert false)
    | None -> Lwt.return_error "end_of_stream"
  in
  match%lwt aux () with
  | Ok (Some s) -> Lwt.return (Stanza s)
  | Ok None -> Lwt.return End
  | Error e -> Lwt.return (Error e)
;;

let parse_string s =
  let parser = create (Lwt_stream.of_string s) in
  let out () =
    match%lwt parse_stanza parser with
    | Stanza s ->
      print_endline (Stanza.pp_to_string s);
      Lwt.return_unit
    | End ->
      print_endline "</stream:stream>";
      Lwt.return_unit
    | Error e ->
      print_endline e;
      Lwt.return_unit
  in
  fun () -> Lwt_main.run (out ())
;;

let to_string _t = "stream and depth"

let%expect_test "initial stanza gets returned" =
  let pf = parse_string "<stream></stream>" in
  pf ();
  [%expect {| <stream/> |}]
;;

let%expect_test "second stanza should get returned too" =
  let pf = parse_string "<stream><second/></stream>" in
  pf ();
  [%expect {| <stream/> |}];
  pf ();
  [%expect {| <second/> |}]
;;

let%expect_test "nested stanza" =
  let pf = parse_string "<stream><body><message>A message!</message></body></stream>" in
  pf ();
  [%expect {| <stream/> |}];
  pf ();
  [%expect
    {|
    <body>
      <message>
        A message!
      </message>
    </body> |}]
;;

let%expect_test "start end full" =
  let pf =
    parse_string
      "<stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' \
       xml:lang='en' xmlns='jabber:client' \
       xmlns:stream='http://etherx.jabber.org/streams'><body>text</body> \
       </stream:stream>"
  in
  pf ();
  [%expect
    {|
    <http://etherx.jabber.org/streams:stream
      from='juliet@im.example.com'
      to='im.example.com'
      version='1.0'
      http://www.w3.org/XML/1998/namespace:lang='en'
      http://www.w3.org/2000/xmlns/:xmlns='jabber:client'
      http://www.w3.org/2000/xmlns/:stream='http://etherx.jabber.org/streams'/> |}];
  pf ();
  [%expect {|
    <jabber:client:body>
      text
    </jabber:client:body> |}];
  pf ();
  [%expect];
  pf ();
  [%expect {| </stream:stream> |}];
  pf ();
  [%expect {| end_of_stream |}]
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
    <http://etherx.jabber.org/streams:stream
      from='juliet@im.example.com'
      to='im.example.com'
      version='1.0'
      http://www.w3.org/XML/1998/namespace:lang='en'
      http://www.w3.org/2000/xmlns/:xmlns='jabber:client'
      http://www.w3.org/2000/xmlns/:stream='http://etherx.jabber.org/streams'/> |}];
  pf ();
  [%expect
    {|
    <jabber:client:iq
      id='redacted_for_testing'
      type='set'>
      <urn:ietf:params:xml:ns:xmpp-bind:bind
      http://www.w3.org/2000/xmlns/:xmlns='urn:ietf:params:xml:ns:xmpp-bind'>
        <urn:ietf:params:xml:ns:xmpp-bind:resource>
          balcony
        </urn:ietf:params:xml:ns:xmpp-bind:resource>
      </urn:ietf:params:xml:ns:xmpp-bind:bind>
    </jabber:client:iq> |}]
;;
