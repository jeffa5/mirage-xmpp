type t =
  { mutable parser : Parser.t
  ; callback : string option -> unit
  ; mutable jid : Jid.t
  ; mutable fsm : State.t
  ; actions_push : Actions.t option -> unit
  ; hostname : string
  ; mutable closed : bool }

let handle_action t stream =
  let rec aux () =
    match%lwt Lwt_stream.get stream with
    | Some action ->
      let open Actions in
      (match action with
      | SEND_STREAM_HEADER ->
        t.callback
          (Some
             (Stream.to_string
                (Header (Stream.create_header ~from:(Jid.of_string t.hostname) ()))))
      | SEND_STREAM_FEATURES_SASL ->
        t.callback (Some (Xml.to_string Stream.features_sasl_mechanisms))
      | SEND_SASL_SUCCESS ->
        t.callback
          (Some
             (Xml.to_string
                (Xml.create
                   (("", "success"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-sasl"]))))
      | SEND_STREAM_FEATURES -> t.callback (Some (Xml.to_string Stream.features))
      | SERVER_GEN_RESOURCE_IDENTIFIER id ->
        let resource = Jid.create_resource () in
        let jid_with_resource = Jid.set_resource resource t.jid in
        t.jid <- jid_with_resource;
        t.callback
          (Some
             (Stanza.to_string (Stanza.create_bind_result ~id ~jid:jid_with_resource ())))
      | SESSION_START_SUCCESS id ->
        t.callback (Some (Stanza.to_string (Stanza.create_iq ~atype:"result" ~id [])))
      | CLOSE ->
        (* After closing the stream we aren't allowed to send anything more so stop handling any more actions *)
        t.callback (Some "</stream:stream>");
        t.closed <- true
      | ERROR e ->
        t.callback (Some e);
        t.closed <- true
      | SET_JID j -> t.jid <- Jid.of_string (j ^ "@" ^ t.hostname)
      | SET_JID_RESOURCE {id; resource} ->
        t.jid <- Jid.set_resource resource t.jid;
        (* send the packet *)
        t.callback
          (Some (Stanza.to_string (Stanza.create_bind_result ~id ~jid:t.jid ())))
      | GET_ROSTER id ->
        let items = Rosters.get_roster_items t.jid in
        t.callback
          (Some (Stanza.to_string (Stanza.create_roster_get_result ~id ~ato:t.jid items)))
      | SET_ROSTER {id; target; handle; groups} ->
        Rosters.set_item ~user:t.jid ~contact:target ~handle ~groups;
        t.callback
          (Some (Stanza.to_string (Stanza.create_roster_set_result ~id ~ato:t.jid)))
      | PUSH_ROSTER {ato; contact} ->
        (match ato with
        | Some (Full_JID _fjid as full_jid) ->
          (match Rosters.get_roster_item full_jid contact with
          | Some item ->
            t.callback
              (Some
                 (Stanza.to_string
                    (Stanza.create_roster_push
                       ~id:(Stanza.gen_id ())
                       ~ato:full_jid
                       (contact, item))))
          | None -> ())
        | None ->
          Connections.find_all (Jid.to_bare t.jid)
          |> List.iter (fun (full_jid, actions_push) ->
                 actions_push (Some (PUSH_ROSTER {ato = Some full_jid; contact})) )
        | _ -> assert false)
      | ADD_TO_CONNECTIONS -> Connections.add t.jid t.actions_push
      | REMOVE_FROM_CONNECTIONS -> Connections.remove t.jid
      | SUBSCRIPTION_REQUEST
          {ato; xml = Xml.Element (((namespace, name), attributes), children); from} ->
        (match Rosters.get_subscription t.jid ato with
        | Some Rosters.To | Some Rosters.Both ->
          let xml =
            Xml.create
              ( ("", "presence")
              , ["", Xml.From ato; "", Xml.To t.jid; "", Xml.Type "subscribed"] )
          in
          t.callback (Some (Xml.to_string xml))
        | _ ->
          Rosters.set_ask t.jid ato;
          let xml =
            Xml.remove_prefixes
              (match from with
              | None ->
                let rec modify_from = function
                  | [] -> ["", Xml.From (t.jid |> Jid.to_bare)]
                  | (ns, Xml.From _) :: attrs ->
                    (ns, Xml.From (t.jid |> Jid.to_bare)) :: attrs
                  | a :: attrs -> a :: modify_from attrs
                in
                Xml.Element (((namespace, name), modify_from attributes), children)
              | Some _ -> Xml.Element (((namespace, name), attributes), children))
          in
          if ato = Jid.to_bare t.jid
          then t.callback (Some (Xml.to_string xml))
          else
            Connections.find_all ato
            |> List.iter (fun (_jid, handler) ->
                   handler
                     (Some
                        (SUBSCRIPTION_REQUEST
                           {ato; xml; from = Some (t.jid |> Jid.to_bare)})) ))
      | SUBSCRIPTION_REQUEST {xml = Xml.Text _; _} -> assert false
      | UPDATE_PRESENCE availability ->
        if Jid.at_least_bare t.jid
        then (
          Rosters.set_presence ~jid:t.jid availability;
          Rosters.get_subscribers t.jid
          |> List.iter (fun jid ->
                 Connections.find_all jid
                 |> List.iter (fun (_, handler) ->
                        handler (Some (SEND_PRESENCE_UPDATE t.jid)) ) ) )
      | SEND_PRESENCE_UPDATE from ->
        let stanza =
          match Rosters.get_presence from with
          | Rosters.Online ->
            Stanza.create_presence ~id:(Some (Stanza.gen_id ())) ~from ~ato:t.jid []
          | Rosters.Offline ->
            Stanza.create_presence
              ~id:(Some (Stanza.gen_id ()))
              ~from
              ~ato:t.jid
              ~atype:"unavailable"
              []
        in
        t.callback (Some (Stanza.to_string stanza))
      | SEND_CURRENT_PRESENCE ato ->
        List.iter (fun (_, handler) ->
            List.iter (fun (jid, _) -> handler (Some (SEND_PRESENCE_UPDATE jid)))
            @@ Connections.find_all (Jid.to_bare t.jid) )
        @@ Connections.find_all ato
      | IQ_ERROR {error_type; error_tag; id} ->
        t.callback
          (Some
             ( Stanza.to_string
             @@ Stanza.create_iq_error
                  ~from:(Jid.of_string t.hostname)
                  ~ato:t.jid
                  ~id
                  ~error_type
                  ~error_tag ))
      | MESSAGE {ato; message} ->
        if ato = t.jid
        then t.callback (Some (Xml.to_string message))
        else (
          match Connections.find ato with
          | Some fn -> fn @@ Some (MESSAGE {ato; message})
          | None -> () )
      | ROSTER_REMOVE {id; target} ->
        Rosters.remove_item t.jid target;
        t.callback
          (Some (Stanza.to_string (Stanza.create_roster_set_result ~id ~ato:t.jid)))
      | SUBSCRIPTION_APPROVAL
          {ato; xml = Xml.Element (((namespace, name), attributes), children); from} ->
        let xml =
          Xml.remove_prefixes
            (match from with
            | None ->
              let rec modify_from = function
                | [] -> ["", Xml.From (t.jid |> Jid.to_bare)]
                | (ns, Xml.From _) :: attrs ->
                  (ns, Xml.From (t.jid |> Jid.to_bare)) :: attrs
                | a :: attrs -> a :: modify_from attrs
              in
              Xml.Element (((namespace, name), modify_from attributes), children)
            | Some _ -> Xml.Element (((namespace, name), attributes), children))
        in
        if ato = Jid.to_bare t.jid
        then t.callback (Some (Xml.to_string xml))
        else (
          match Rosters.get_subscription ato t.jid with
          | Some Rosters.None | Some Rosters.From ->
            if Rosters.get_ask ato t.jid
            then (
              Rosters.upgrade_subscription_to ato t.jid;
              Rosters.unset_ask ato t.jid;
              Connections.find_all ato
              |> List.iter (fun (jid, handler) ->
                     handler
                       (Some
                          (SUBSCRIPTION_APPROVAL
                             {ato; xml; from = Some (t.jid |> Jid.to_bare)}));
                     handler
                       (Some (PUSH_ROSTER {ato = Some jid; contact = Jid.to_bare t.jid}))
                 ) )
          | _ -> () )
      | SUBSCRIPTION_APPROVAL {xml = Xml.Text _; _} -> assert false
      | ROSTER_SET_FROM from -> Rosters.upgrade_subscription_from t.jid from);
      if t.closed
      then (
        t.callback None;
        Lwt.return_unit )
      else aux ()
    | None ->
      t.callback None;
      Lwt.return_unit
  in
  aux ()
;;

let create ~stream ~callback ~hostname =
  let parser = Parser.create stream in
  let jid = Jid.empty in
  let fsm = State.initial in
  let actions_stream, actions_push = Lwt_stream.create () in
  let t = {parser; callback; jid; fsm; actions_push; hostname; closed = false} in
  Lwt.async (fun () -> handle_action t actions_stream);
  t
;;

let handle t =
  let rec aux () =
    let%lwt parse_result = Parser.parse t.parser in
    let event = Events.lift parse_result in
    let new_fsm, actions, handler_actions = State.handle t.fsm event in
    t.fsm <- new_fsm;
    List.iter
      (function Actions.RESET_PARSER -> t.parser <- Parser.reset t.parser)
      handler_actions;
    List.iter (fun action -> t.actions_push (Some action)) actions;
    if t.closed then Lwt.return_unit else aux ()
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
    | None -> print_endline "Out stream closed"
  in
  create ~stream ~callback ~hostname:"im.example.com"
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
    {state: IDLE}
    closed:
    false
    } |}]
;;

let%expect_test "initial stanza with version" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
    <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
    <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism></mechanisms></stream:features>
    Unexpected stream close during sasl negotiation
    Out stream closed
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
    empty
    fsm:
    {state: CLOSED}
    closed:
    true
    } |}]
;;

let%expect_test "error in initial stanza" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
    <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
    <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism></mechanisms></stream:features>
    Unexpected stream close during sasl negotiation
    Out stream closed |}];
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
    {state: CLOSED}
    closed:
    true
    } |}]
;;

let%expect_test "bind resource" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
    ^ Xml.to_string
        (Xml.create
           ( ("", "auth")
           , ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-sasl"; "", Xml.Mechanism "PLAIN"]
           )
           ~children:[Xml.Text "AGp1bGlldABwYXNzd29yZA=="])
    ^ Stream.to_string
        (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
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
    <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
    <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism></mechanisms></stream:features>
    <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>
    <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
    <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
    <iq id='<redacted_for_testing>' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
    </stream:stream>
    Out stream closed |}];
  print_endline (to_string handler);
  [%expect
    {|
    {
    rosters:
    [juliet@im.example.com: Offline; ]
    connections:
    []
    parser:
    callback
    jid:
    juliet@im.example.com/balcony
    fsm:
    {state: CLOSED}
    closed:
    true
    } |}]
;;

let%expect_test "roster get" =
  Rosters.clear ();
  Connections.clear ();
  let stanza =
    Stream.to_string
      (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
    ^ Xml.to_string
        (Xml.create
           ( ("", "auth")
           , ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-sasl"; "", Xml.Mechanism "PLAIN"]
           )
           ~children:[Xml.Text "AGp1bGlldABwYXNzd29yZA=="])
    ^ Stream.to_string
        (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
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
      <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
      <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism></mechanisms></stream:features>
      <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>
      <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
      <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
      <iq id='<redacted_for_testing>' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
      <iq id='<redacted_for_testing>' type='result' to='juliet@im.example.com/balcony'><query xmlns='jabber:iq:roster'/></iq>
      </stream:stream>
      Out stream closed
    |}];
  print_endline (to_string handler);
  [%expect
    {|
      {
      rosters:
      [juliet@im.example.com: Offline; ]
      connections:
      []
      parser:
      callback
      jid:
      juliet@im.example.com/balcony
      fsm:
      {state: CLOSED}
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
      (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
    ^ Xml.to_string
        (Xml.create
           ( ("", "auth")
           , ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-sasl"; "", Xml.Mechanism "PLAIN"]
           )
           ~children:[Xml.Text "AGp1bGlldABwYXNzd29yZA=="])
    ^ Stream.to_string
        (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
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
             ; "", Xml.From (Jid.of_string "juliet@im.example.com/balcony")
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
             ; "", Xml.From (Jid.of_string "juliet@im.example.com/balcony") ] )
           ~children:
             [Xml.create (("", "query"), ["", Xml.Xmlns "jabber:iq:roster"]) ~children:[]])
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
      <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
      <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism></mechanisms></stream:features>
      <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>
      <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
      <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
      <iq id='<redacted_for_testing>' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
      <iq id='<redacted_for_testing>' type='result' to='juliet@im.example.com/balcony'/>
      <iq id='<redacted_for_testing>' type='set' to='juliet@im.example.com/balcony'><query xmlns='jabber:iq:roster'><item jid='nurse@example.com' subscription='none' name='Nurse'><group>Servants</group></item></query></iq>
      <iq id='<redacted_for_testing>' type='result' to='juliet@im.example.com/balcony'><query xmlns='jabber:iq:roster'><item jid='nurse@example.com' name='Nurse' subscription='none'><group>Servants</group></item></query></iq>
      </stream:stream>
      Out stream closed
    |}];
  print_endline (to_string handler);
  [%expect
    {|
      {
      rosters:
      [juliet@im.example.com: Offline; nurse@example.com: {Nurse; none; false; [Servants]}]
      connections:
      []
      parser:
      callback
      jid:
      juliet@im.example.com/balcony
      fsm:
      {state: CLOSED}
      closed:
      true
      }
     |}]
;;
