type t =
  { parser : Parser.t
  ; callback : string option -> unit
  ; mutable jid : Jid.t
  ; mutable fsm : State.t
  ; actions_push : Actions.t option -> unit }

let handle_action t stream =
  let rec aux () =
    match%lwt Lwt_stream.get stream with
    | Some action ->
      let open Actions in
      let closed =
        match action with
        | SEND_STREAM_HEADER tag ->
          t.callback (Some (Stream.to_string (Header tag)));
          false
        | SEND_STREAM_FEATURES xml ->
          t.callback (Some (Xml.to_string xml));
          false
        | REPLY_STANZA s ->
          t.callback (Some (Stanza.to_string s));
          false
        | CLOSE ->
          (* After closing the stream we aren't allowed to send anything more so stop handling any more actions *)
          t.callback (Some "</stream:stream>");
          true
        | ERROR e ->
          t.callback (Some e);
          true
        | SET_JID j ->
          t.jid <- j;
          false
        | SET_JID_RESOURCE (id, res) ->
          t.jid <- Jid.set_resource res t.jid;
          (* send the packet *)
          t.callback
            (Some
               (Stanza.to_string
                  (Stanza.create_iq_bind
                     id
                     ~children:
                       [ Xml.create
                           (("", "jid"), [])
                           ~children:[Xml.Text (Jid.to_string t.jid)] ])));
          false
        | GET_ROSTER (id, from) ->
          let items = Rosters.get_jids from in
          t.callback
            (Some
               (Stanza.to_string
                  (Stanza.create_iq_query
                     ~id
                     ~from
                     ~children:
                       (List.map
                          (fun jid -> Xml.create (("", "item"), ["", Xml.Jid jid]))
                          items)
                     ())));
          false
        | SET_ROSTER (id, from, target, handle, subscribed, groups) ->
          Rosters.set_item ~user_jid:from ~target_jid:target ~handle ~subscribed ~groups;
          t.callback
            (Some
               (Stanza.to_string
                  (Stanza.create_iq
                     ["", Xml.Id id; "", Xml.To from; "", Xml.Type "result"])));
          false
        | PUSH_ROSTER (jid, updated_jid) ->
          (match jid with
          | Full_JID _fjid as full_jid ->
            let item = Xml.create (("", "item"), ["", Xml.Jid updated_jid]) in
            t.callback
              (Some
                 (Stanza.to_string
                    (Stanza.create_iq_query
                       ~id:(Stanza.gen_id ())
                       ~from:(Jid.to_bare full_jid)
                       ~attributes:["", Xml.To full_jid; "", Xml.Type "set"]
                       ~children:[item]
                       ())))
          | Bare_JID _bjid as bare_jid ->
            Connections.find_all bare_jid
            |> List.iter (fun (target_jid, actions_push) ->
                   actions_push (Some (PUSH_ROSTER (target_jid, updated_jid))) )
          | _ -> assert false);
          false
        | ADD_TO_CONNECTIONS ->
          Connections.add t.jid t.actions_push;
          false
        | REMOVE_FROM_CONNECTIONS ->
          Connections.remove t.jid;
          false
      in
      if closed then Lwt.return_unit else aux ()
    | None ->
      t.callback None;
      Lwt.return_unit
  in
  aux ()
;;

let create ~stream ~callback =
  let parser = Parser.create stream in
  let jid = Jid.empty in
  let fsm = State.create () in
  let actions_stream, actions_push = Lwt_stream.create () in
  let t = {parser; callback; jid; fsm; actions_push} in
  Lwt.async (fun () -> handle_action t actions_stream);
  t
;;

let handle t =
  let rec aux () =
    let%lwt parse_result = Parser.parse t.parser in
    let event = Events.lift parse_result in
    let new_fsm, actions = State.handle t.fsm event in
    t.fsm <- new_fsm;
    List.iter (fun action -> t.actions_push (Some action)) actions;
    if State.closed t.fsm then Lwt.return_unit else aux ()
  in
  aux ()
;;

let to_string t =
  String.concat
    "\n"
    [ "{"
    ; "rosters: "
    ; Rosters.to_string ()
    ; "connections: "
    ; Connections.to_string ()
    ; "parser: "
    ; "callback"
    ; "jid: "
    ; Jid.to_string t.jid
    ; "fsm: "
    ; State.to_string t.fsm
    ; "}" ]
;;

let make_test_handler s =
  let mask_id s =
    match Astring.String.find_sub ~sub:"id='" s with
    | Some i ->
      (match Astring.String.find_sub ~start:(i + 4) ~sub:"'" s with
      | Some j ->
        Astring.String.with_index_range ~first:0 ~last:(i + 3) s
        ^ "redacted_for_testing"
        ^ Astring.String.with_index_range ~first:j s
      | None -> assert false)
    | None -> s
  in
  let stream = Lwt_stream.of_string s in
  let callback so =
    match so with
    | Some s -> print_endline (mask_id s)
    | None -> print_endline "received None in callback"
  in
  create ~stream ~callback
;;

let test_stanza stanza =
  let handler = make_test_handler stanza in
  let run = handle handler in
  Lwt_main.run run;
  handler
;;

let%expect_test "creation of handler" =
  Rosters.clear ();
  Connections.clear ();
  let handler = make_test_handler "<stream></stream>" in
  print_endline (to_string handler);
  [%expect
    {|
    {
    rosters:
    []
    connections:
    []
    parser:
    callback
    jid:
    empty
    fsm:
    {state: idle}
    } |}]
;;

let%expect_test "initial stanza with version" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header
         (Stream.create_header
            (Jid.of_string "juliet@im.example.com")
            (Jid.of_string "im.example.com")))
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
    </stream:stream>
    |}];
  print_endline (to_string handler);
  [%expect
    {|
    {
    rosters:
    []
    connections:
    []
    parser:
    callback
    jid:
    juliet@im.example.com
    fsm:
    {state: closed}
    } |}]
;;

let%expect_test "error in initial stanza" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header
         (Stream.create_header
            (Jid.of_string "juliet@im.example.com")
            (Jid.of_string "im.example.com")))
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
    </stream:stream> |}];
  print_endline (to_string handler);
  [%expect
    {|
    {
    rosters:
    []
    connections:
    []
    parser:
    callback
    jid:
    juliet@im.example.com
    fsm:
    {state: closed}
    } |}]
;;

let%expect_test "bind resource" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header
         (Stream.create_header
            (Jid.of_string "juliet@im.example.com")
            (Jid.of_string "im.example.com")))
    ^ Xml.to_string
        (Xml.create
           (("", "iq"), ["", Xml.Id "some_id"; "", Xml.Type "set"])
           ~children:
             [ Xml.create
                 (("", "bind"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-bind"])
                 ~children:
                   [Xml.create (("", "resource"), []) ~children:[Xml.Text "balcony"]] ])
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
    <iq id='redacted_for_testing' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
    </stream:stream> |}];
  print_endline (to_string handler);
  [%expect
    {|
    {
    rosters:
    []
    connections:
    []
    parser:
    callback
    jid:
    juliet@im.example.com/balcony
    fsm:
    {state: closed}
    } |}]
;;

let%expect_test "roster get" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header
         (Stream.create_header
            (Jid.of_string "juliet@im.example.com")
            (Jid.of_string "im.example.com")))
    ^ Xml.to_string
        (Xml.create
           (("", "iq"), ["", Xml.Id "some_id"; "", Xml.Type "set"])
           ~children:
             [ Xml.create
                 (("", "bind"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-bind"])
                 ~children:
                   [Xml.create (("", "resource"), []) ~children:[Xml.Text "balcony"]] ])
    ^ Xml.to_string
        (Xml.create
           ( ("", "iq")
           , [ "", Xml.Id "some_id"
             ; "", Xml.Type "get"
             ; "", Xml.From (Jid.of_string "juliet@example.com/balcony") ] )
           ~children:
             [Xml.create (("", "query"), ["", Xml.Xmlns "jabber:iq:roster"]) ~children:[]])
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
      <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
      <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
      <iq id='redacted_for_testing' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
      <iq id='redacted_for_testing' to='juliet@example.com/balcony' type='result'><query xmlns='jabber:iq:roster' ver='ver7'/></iq>
      </stream:stream>
    |}];
  print_endline (to_string handler);
  [%expect
    {|
      {
      rosters:
      []
      connections:
      []
      parser:
      callback
      jid:
      juliet@im.example.com/balcony
      fsm:
      {state: closed}
      }
     |}]
;;

let%expect_test "roster set" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header
         (Stream.create_header
            (Jid.of_string "juliet@im.example.com")
            (Jid.of_string "im.example.com")))
    ^ Xml.to_string
        (Xml.create
           (("", "iq"), ["", Xml.Id "some_id"; "", Xml.Type "set"])
           ~children:
             [ Xml.create
                 (("", "bind"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-bind"])
                 ~children:
                   [Xml.create (("", "resource"), []) ~children:[Xml.Text "balcony"]] ])
    ^ Xml.to_string
        (Xml.create
           ( ("", "iq")
           , [ "", Xml.Id "ph1xaz53"
             ; "", Xml.From (Jid.of_string "juliet@example.com/balcony")
             ; "", Xml.Type "set" ] )
           ~children:
             [ Xml.create
                 (("", "query"), ["", Xml.Xmlns "jabber:iq:roster"])
                 ~children:
                   [ Xml.create
                       ( ("", "item")
                       , [ "", Xml.Jid (Jid.of_string "nurse@example.com")
                         ; "", Xml.Name "Nurse" ] )
                       ~children:
                         [Xml.create (("", "group"), []) ~children:[Xml.Text "Servants"]]
                   ] ])
    ^ Xml.to_string
        (Xml.create
           ( ("", "iq")
           , [ "", Xml.Id "some_id"
             ; "", Xml.Type "get"
             ; "", Xml.From (Jid.of_string "juliet@example.com/balcony") ] )
           ~children:
             [Xml.create (("", "query"), ["", Xml.Xmlns "jabber:iq:roster"]) ~children:[]])
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
      <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
      <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
      <iq id='redacted_for_testing' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
      <iq id='redacted_for_testing' to='juliet@example.com/balcony' type='result'/>
      <iq id='redacted_for_testing' to='juliet@example.com/balcony' type='result'><query xmlns='jabber:iq:roster' ver='ver7'><item jid='nurse@example.com'/></query></iq>
      </stream:stream>
    |}];
  print_endline (to_string handler);
  [%expect
    {|
      {
      rosters:
      [juliet@example.com/balcony: false; nurse@example.com: {Nurse; false; [Servants]}]
      connections:
      []
      parser:
      callback
      jid:
      juliet@im.example.com/balcony
      fsm:
      {state: closed}
      }
     |}]
;;
