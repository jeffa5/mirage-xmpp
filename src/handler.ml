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
        let contact = Jid.to_bare contact in
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
          | None ->
            t.callback
              (Some
                 (Stanza.to_string
                    (Stanza.create_roster_push
                       ~id:(Stanza.gen_id ())
                       ~ato:full_jid
                       ( contact
                       , {handle = ""; subscription = Remove; ask = false; groups = []}
                       )))))
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
      | UPDATE_PRESENCE {status; xml} ->
        if Jid.at_least_bare t.jid
        then (
          Rosters.set_presence ~jid:t.jid status;
          [Jid.to_bare t.jid] @ Rosters.get_subscribers t.jid
          |> List.iter (fun jid ->
                 Connections.find_all jid
                 |> List.iter (fun (_, handler) ->
                        handler (Some (SEND_PRESENCE_UPDATE {from = t.jid; xml})) ) ) )
      | SEND_PRESENCE_UPDATE {from; xml} ->
        let rec modify_from = function
          | [] -> ["", Xml.From from]
          | (ns, Xml.From _) :: attrs -> (ns, Xml.From from) :: attrs
          | a :: attrs -> a :: modify_from attrs
        in
        let rec modify_to = function
          | [] -> ["", Xml.To (t.jid |> Jid.to_bare)]
          | (ns, Xml.To _) :: attrs -> (ns, Xml.To (t.jid |> Jid.to_bare)) :: attrs
          | a :: attrs -> a :: modify_to attrs
        in
        let stanza =
          match xml with
          | Some (Xml.Element (((ns, n), attributes), c)) ->
            Stanza.Presence
              ( Xml.remove_prefixes
              @@ Xml.Element (((ns, n), modify_to @@ modify_from attributes), c) )
          | Some (Xml.Text _) -> assert false
          | None ->
            (match Rosters.get_presence from with
            | Rosters.Online ->
              Stanza.create_presence ~id:(Some (Stanza.gen_id ())) ~from ~ato:t.jid []
            | Rosters.Offline ->
              Stanza.create_presence
                ~id:(Some (Stanza.gen_id ()))
                ~from
                ~ato:t.jid
                ~atype:"unavailable"
                [])
        in
        t.callback (Some (Stanza.to_string stanza))
      | SEND_CURRENT_PRESENCE ato ->
        Connections.find_all ato
        |> List.iter (fun (_, handler) ->
               Connections.find_all (Jid.to_bare t.jid)
               |> List.iter (fun (jid, _) ->
                      handler (Some (SEND_PRESENCE_UPDATE {from = jid; xml = None})) ) )
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
      | MESSAGE
          { ato
          ; message = Xml.Element (((namespace, name), attributes), children) as message
          } ->
        if Jid.to_bare ato = Jid.to_bare t.jid
        then t.callback (Some (Xml.to_string message))
        else
          let message =
            Xml.Element
              (((namespace, name), ("", Xml.From t.jid) :: attributes), children)
          in
          Connections.find_all ato
          |> List.iter (fun (_, handler) -> handler (Some (MESSAGE {ato; message})))
      | MESSAGE {message = Xml.Text _; _} -> assert false
      | ROSTER_REMOVE {id; target} ->
        let unsubscribe () =
          t.actions_push (Some (SUBSCRIPTION_REMOVAL {contact = Jid.to_bare target}))
        in
        let unsubscribed () =
          t.actions_push
            (Some (SUBSCRIPTION_CANCELLATION {user = Jid.to_bare target; force = true}))
        in
        (match Rosters.get_subscription t.jid target with
        | Some Rosters.Both ->
          unsubscribe ();
          unsubscribed ()
        | Some Rosters.To -> unsubscribe ()
        | Some Rosters.From -> unsubscribed ()
        | Some Rosters.None | Some Rosters.Remove -> ()
        | None -> ());
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
      | ROSTER_SET_FROM from -> Rosters.upgrade_subscription_from t.jid from
      | PROBE_PRESENCE ->
        (Connections.find_all (Jid.to_bare t.jid) |> List.map (fun (jid, _) -> jid))
        @ List.fold_left
            (fun l jid -> l @ List.map (fun (jid, _) -> jid) @@ Connections.find_all jid)
            []
            (Rosters.get_subscriptions t.jid)
        |> List.iter (fun jid ->
               t.actions_push (Some (SEND_PRESENCE_UPDATE {from = jid; xml = None})) )
      | SUBSCRIPTION_CANCELLATION {user; force} ->
        let contact = t.jid in
        let aux () =
          (match Rosters.get_roster_item user contact with
          | Some _ ->
            Connections.find_all contact
            |> List.iter (fun (from, _) ->
                   Connections.find_all user
                   |> List.iter (fun (_, handler) ->
                          handler
                            (Some
                               (SEND_PRESENCE_UPDATE
                                  { from
                                  ; xml =
                                      Some
                                        ( Stanza.to_xml
                                        @@ Stanza.create_presence
                                             ~id:(Some (Stanza.gen_id ()))
                                             ~from
                                             ~atype:"unavailable"
                                             [] ) })) ) );
            Connections.find_all user
            |> List.iter (fun (jid, handler) ->
                   handler
                     (Some
                        (SEND_PRESENCE_UPDATE
                           { from = Jid.to_bare t.jid
                           ; xml =
                               Some
                                 ( Stanza.to_xml
                                 @@ Stanza.create_presence
                                      ~id:(Some (Stanza.gen_id ()))
                                      ~from:(Jid.to_bare t.jid)
                                      ~ato:(Jid.to_bare jid)
                                      ~atype:"unsubscribed"
                                      [] ) })) );
            Rosters.downgrade_subscription_to user contact;
            Connections.find_all user
            |> List.iter (fun (jid, handler) ->
                   handler (Some (PUSH_ROSTER {ato = Some jid; contact})) )
          | None -> ());
          Rosters.downgrade_subscription_from contact user;
          t.actions_push (Some (PUSH_ROSTER {ato = None; contact = user}))
        in
        if force
        then aux ()
        else (
          match Rosters.get_subscription contact user with
          | Some From | Some Both -> aux ()
          | _ -> () )
      | SUBSCRIPTION_REMOVAL {contact} ->
        let user = Jid.to_bare t.jid in
        (match Rosters.get_subscription contact user with
        | Some Rosters.From | Some Rosters.Both ->
          (* deliver unsubscribe stanza to all contacts resources *)
          Connections.find_all contact
          |> List.iter (fun (jid, handler) ->
                 handler
                   (Some
                      (SEND_PRESENCE_UPDATE
                         { from = user
                         ; xml =
                             Some
                               ( Stanza.to_xml
                               @@ Stanza.create_presence
                                    ~id:(Some (Stanza.gen_id ()))
                                    ~from:user
                                    ~ato:jid
                                    ~atype:"unsubscribe"
                                    [] ) })) );
          (* roster push to contacts resources with sub=none / to *)
          Rosters.downgrade_subscription_from contact user;
          Connections.find_all contact
          |> List.iter (fun (jid, handler) ->
                 handler (Some (PUSH_ROSTER {ato = Some jid; contact = user})) );
          (* unavailable from contacts resources to bare user *)
          Connections.find_all contact
          |> List.iter (fun (jid, _) ->
                 Connections.find_all user
                 |> List.iter (fun (_, handler) ->
                        handler
                          (Some
                             (SEND_PRESENCE_UPDATE
                                { from = jid
                                ; xml =
                                    Some
                                      ( Stanza.to_xml
                                      @@ Stanza.create_presence
                                           ~id:(Some (Stanza.gen_id ()))
                                           ~from:jid
                                           ~ato:user
                                           ~atype:"unavailable"
                                           [] ) })) ) )
        | _ -> ());
        (* Roster push to users resources with none/from *)
        Rosters.downgrade_subscription_to user contact;
        Connections.find_all user
        |> List.iter (fun (jid, handler) ->
               handler (Some (PUSH_ROSTER {ato = Some jid; contact})) ));
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
  let closed = ref false in
  let rec aux () =
    let%lwt parse_result = Parser.parse t.parser in
    let event = Events.lift parse_result in
    let new_fsm, actions, handler_actions = State.handle t.fsm event in
    t.fsm <- new_fsm;
    List.iter
      (function
        | Actions.RESET_PARSER -> t.parser <- Parser.reset t.parser
        | Actions.EXIT -> closed := true)
      handler_actions;
    List.iter (fun action -> t.actions_push (Some action)) actions;
    if !closed then Lwt.return_unit else aux ()
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
      <presence from='juliet@im.example.com/balcony' to='juliet@im.example.com/balcony' id='<redacted_for_testing>' type='unavailable'/>
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
      <presence from='juliet@im.example.com/balcony' to='juliet@im.example.com/balcony' id='<redacted_for_testing>' type='unavailable'/>
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
