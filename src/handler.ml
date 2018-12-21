type t =
  { parser : Parser.t
  ; callback : string option -> unit
  ; mutable jid : Jid.t
  ; mutable fsm : State.t
  ; actions_push : Actions.t option -> unit
  ; mutable closed : bool }

let handle_action t stream =
  let rec aux () =
    match%lwt Lwt_stream.get stream with
    | Some action ->
      let open Actions in
      (match action with
      | SEND_STREAM_HEADER {from; ato} ->
        t.callback (Some (Stream.to_string (Header (Stream.create_header ato from))))
      | SEND_STREAM_FEATURES -> t.callback (Some (Xml.to_string Stream.features))
      | REPLY_STANZA s -> t.callback (Some (Stanza.to_string s))
      | SERVER_GEN_RESOURCE_IDENTIFIER id ->
        t.callback
          (Some
             (Stanza.to_string
                (Stanza.create_bind_result
                   ~id
                   ~jid:(Jid.set_resource (Jid.create_resource ()) t.jid)
                   ())))
      | CLOSE ->
        (* After closing the stream we aren't allowed to send anything more so stop handling any more actions *)
        t.callback (Some "</stream:stream>");
        t.closed <- true
      | ERROR e ->
        t.callback (Some e);
        t.closed <- true
      | SET_JID j -> t.jid <- j
      | SET_JID_RESOURCE {id; resource} ->
        t.jid <- Jid.set_resource resource t.jid;
        (* send the packet *)
        t.callback
          (Some (Stanza.to_string (Stanza.create_bind_result ~id ~jid:t.jid ())))
      | GET_ROSTER {id; from} ->
        let items = Rosters.get from in
        t.callback
          (Some (Stanza.to_string (Stanza.create_roster_get_result ~id ~ato:from items)))
      | SET_ROSTER {id; from; target; handle; subscription; groups} ->
        Rosters.set_item ~user_jid:from ~target_jid:target ~handle ~subscription ~groups;
        t.callback
          (Some (Stanza.to_string (Stanza.create_roster_set_result ~id ~ato:from)))
      | PUSH_ROSTER {jid; updated_jid} ->
        (match jid with
        | Full_JID _fjid as full_jid ->
          t.callback
            (Some
               (Stanza.to_string
                  (Stanza.create_roster_push
                     ~id:(Stanza.gen_id ())
                     ~ato:full_jid
                     ~jid:updated_jid)))
        | Bare_JID _bjid as bare_jid ->
          Connections.find_all bare_jid
          |> List.iter (fun (target_jid, actions_push) ->
                 actions_push (Some (PUSH_ROSTER {jid = target_jid; updated_jid})) )
        | _ -> assert false)
      | ADD_TO_CONNECTIONS -> Connections.add t.jid t.actions_push
      | REMOVE_FROM_CONNECTIONS -> Connections.remove t.jid);
      if t.closed then Lwt.return_unit else aux ()
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
  let t = {parser; callback; jid; fsm; actions_push; closed = false} in
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
    ; "closed: "
    ; string_of_bool t.closed
    ; "}" ]
;;

let make_test_handler s =
  let stream = Lwt_stream.of_string s in
  let callback so =
    match so with
    | Some s -> print_endline (Utils.mask_id s)
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
    closed:
    false
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
    closed:
    true
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
    closed:
    true
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
    ^ Stanza.to_string
        (Stanza.create_iq
           ~id:(Stanza.gen_id ())
           ~atype:"set"
           [Stanza.create_bind [Stanza.create_resource [Xml.Text "balcony"]]])
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
    closed:
    true
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
      <iq id='redacted_for_testing' type='result' to='juliet@example.com/balcony'><query xmlns='jabber:iq:roster'/></iq>
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
      closed:
      true
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
      <iq id='redacted_for_testing' type='result' to='juliet@example.com/balcony'/>
      <iq id='redacted_for_testing' type='result' to='juliet@example.com/balcony'><query xmlns='jabber:iq:roster'><item jid='nurse@example.com' name='Nurse' subscription='none'><group>Servants</group></item></query></iq>
      </stream:stream>
    |}];
  print_endline (to_string handler);
  [%expect
    {|
      {
      rosters:
      [juliet@example.com/balcony: false; nurse@example.com: {Nurse; none; [Servants]}]
      connections:
      []
      parser:
      callback
      jid:
      juliet@im.example.com/balcony
      fsm:
      {state: closed}
      closed:
      true
      }
     |}]
;;
