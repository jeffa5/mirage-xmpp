open Sexplib.Std

type t =
  { mutable parser : Parser.t
  ; callback : string option -> unit
  ; mutable user : string option
  ; hostname : string
  ; mutable jid : Jid.Full.t option
  ; mutable fsm : State.t
  ; actions_push : Actions.t option -> unit
  ; mutable closed : bool }
[@@deriving sexp]

let handle_action t stream =
  let rec aux () =
    match%lwt Lwt_stream.get stream with
    | Some action ->
      let open Actions in
      let%lwt () =
        match action with
        | SEND_STREAM_HEADER ->
          let header =
            Stream.Header (Stream.create_header ~from:(Jid.of_string t.hostname) ())
          in
          let xml_string = Stream.to_string header in
          t.callback (Some xml_string) |> Lwt.return
        | SEND_STREAM_FEATURES_SASL ->
          let features = Stream.features_sasl_mechanisms in
          let xml_string = Xml.to_string features in
          t.callback (Some xml_string) |> Lwt.return
        | SEND_STREAM_FEATURES ->
          let features = Stream.features in
          let xml_string = Xml.to_string features in
          t.callback (Some xml_string) |> Lwt.return
        | SEND_SASL_SUCCESS ->
          let success =
            Xml.create
              (("", "success"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-sasl"])
          in
          let xml_string = Xml.to_string success in
          t.callback (Some xml_string) |> Lwt.return
        | SESSION_START_SUCCESS id ->
          let iq = Stanza.create_iq ~atype:"result" ~id [] in
          let xml_string = Stanza.to_string iq in
          t.callback (Some xml_string) |> Lwt.return
        | SET_USER user -> (t.user <- Some user) |> Lwt.return
        | SET_USER_ANON -> (t.user <- Some (Jid.anon ())) |> Lwt.return
        | SET_JID_RESOURCE {id; resource} ->
          (match t.user with
          | Some user ->
            let resource =
              match resource with Some r -> r | None -> Jid.create_resource ()
            in
            let jid_with_resource =
              Jid.Full.of_string (user ^ "@" ^ t.hostname ^ "/" ^ resource)
            in
            t.jid <- Some jid_with_resource;
            let bind_result =
              Stanza.create_bind_result ~id ~jid:(Full_JID jid_with_resource) ()
            in
            let xml_string = Stanza.to_string bind_result in
            t.callback (Some xml_string) |> Lwt.return
          | None -> Lwt.return_unit)
        | GET_ROSTER id ->
          (match t.jid with
          | Some jid ->
            let%lwt items = Rosters.get_items (Jid.Full.to_bare jid) in
            let roster_get_result =
              Stanza.create_roster_get_result ~id ~ato:(Full_JID jid) items
            in
            let xml_string = Stanza.to_string roster_get_result in
            t.callback (Some xml_string) |> Lwt.return
          | None -> Lwt.return_unit)
        | SET_ROSTER {id; target; handle; groups} ->
          (match t.jid with
          | Some jid ->
            let%lwt () =
              Rosters.set_item ~handle ~groups (Jid.Full.to_bare jid) target
            in
            let roster_set_result =
              Stanza.create_roster_set_result ~id ~ato:(Full_JID jid)
            in
            let xml_string = Stanza.to_string roster_set_result in
            t.callback (Some xml_string) |> Lwt.return
          | None -> Lwt.return_unit)
        | PUSH_ROSTER {ato; contact} ->
          let contact = Jid.to_bare contact in
          (match ato with
          | Some (Full_JID _ as full_jid) ->
            (match%lwt
               Rosters.get_item (Jid.to_bare_raw full_jid) (Jid.to_bare_raw contact)
             with
            | Some item ->
              let roster_push =
                Stanza.create_roster_push
                  ~id:(Stanza.gen_id ())
                  ~ato:full_jid
                  (contact, item)
              in
              let xml_string = Stanza.to_string roster_push in
              t.callback (Some xml_string) |> Lwt.return
            | None ->
              let roster_push =
                Stanza.create_roster_push
                  ~id:(Stanza.gen_id ())
                  ~ato:full_jid
                  ( contact
                  , Rosters.Item.make
                      ~handle:""
                      ~subscription:Rosters.Subscription.Remove
                      () )
              in
              let xml_string = Stanza.to_string roster_push in
              t.callback (Some xml_string) |> Lwt.return)
          | None ->
            (match t.jid with
            | Some jid ->
              let%lwt connected_resources =
                Connections.find_all (Jid.Full.to_bare jid)
              in
              Lwt_list.iter_s
                (fun (full_jid, actions_push) ->
                  actions_push
                    (Some (PUSH_ROSTER {ato = Some (Full_JID full_jid); contact}));
                  Lwt.return_unit )
                connected_resources
            | None -> Lwt.return_unit)
          | _ -> assert false)
        | ROSTER_REMOVE {id; target} ->
          (match t.jid with
          | Some jid ->
            let unsubscribe () =
              t.actions_push (Some (SUBSCRIPTION_REMOVAL {contact = Jid.to_bare target}))
            in
            let unsubscribed () =
              t.actions_push
                (Some
                   (SUBSCRIPTION_CANCELLATION {user = Jid.to_bare target; force = true}))
            in
            let%lwt subscription =
              Rosters.get_subscription (Jid.Full.to_bare jid) (Jid.to_bare_raw target)
            in
            (match subscription with
            | Some Both ->
              unsubscribe ();
              unsubscribed ()
            | Some To -> unsubscribe ()
            | Some From -> unsubscribed ()
            | Some None | Some Remove | None -> ());
            let%lwt () =
              Rosters.remove_item (Jid.Full.to_bare jid) (Jid.to_bare_raw target)
            in
            let roster_set_result =
              Stanza.create_roster_set_result ~id ~ato:(Full_JID jid)
            in
            let xml_string = Stanza.to_string roster_set_result in
            t.callback (Some xml_string) |> Lwt.return
          | None -> Lwt.return_unit)
        | ROSTER_SET_FROM from ->
          (match t.jid with
          | Some jid ->
            Rosters.upgrade_subscription_from
              (Jid.Full.to_bare jid)
              (Jid.to_bare_raw from)
          | None -> Lwt.return_unit)
        | ADD_TO_CONNECTIONS ->
          (match t.jid with
          | Some jid -> Connections.add jid t.actions_push
          | None -> Lwt.return_unit)
        | REMOVE_FROM_CONNECTIONS ->
          (match t.jid with
          | Some jid -> Connections.remove jid
          | None -> Lwt.return_unit)
        | SUBSCRIPTION_REQUEST {ato; xml; from} ->
          (match xml with
          | Xml.Element (((namespace, name), attributes), children) ->
            (match t.jid with
            | Some jid ->
              (match%lwt
                 Rosters.get_subscription (Jid.Full.to_bare jid) (Jid.to_bare_raw ato)
               with
              | Some To | Some Both ->
                let presence_subscribed =
                  Xml.create
                    ( ("", "presence")
                    , [ "", Xml.From ato
                      ; "", Xml.To (Full_JID jid)
                      ; "", Xml.Type "subscribed" ] )
                in
                let xml_string = Xml.to_string presence_subscribed in
                t.callback (Some xml_string) |> Lwt.return
              | _ ->
                let%lwt () =
                  Rosters.set_ask (Jid.Full.to_bare jid) (Jid.to_bare_raw ato)
                in
                let xml =
                  Xml.remove_prefixes
                    (match from with
                    | None ->
                      let rec modify_from = function
                        | [] -> ["", Xml.From (Bare_JID (Jid.Full.to_bare jid))]
                        | (ns, Xml.From _) :: attrs ->
                          (ns, Xml.From (Bare_JID (Jid.Full.to_bare jid))) :: attrs
                        | a :: attrs -> a :: modify_from attrs
                      in
                      Xml.Element (((namespace, name), modify_from attributes), children)
                    | Some _ -> Xml.Element (((namespace, name), attributes), children))
                in
                if ato = Bare_JID (Jid.Full.to_bare jid)
                then t.callback (Some (Xml.to_string xml)) |> Lwt.return
                else
                  let%lwt connected_resources =
                    Connections.find_all (Jid.to_bare_raw ato)
                  in
                  Lwt_list.iter_s
                    (fun (_jid, handler) ->
                      handler
                        (Some
                           (SUBSCRIPTION_REQUEST
                              {ato; xml; from = Some (Bare_JID (Jid.Full.to_bare jid))}))
                      |> Lwt.return )
                    connected_resources)
            | None -> Lwt.return_unit)
          | Xml.Text _ -> assert false)
        | SUBSCRIPTION_APPROVAL {ato; xml; from} ->
          (match xml with
          | Xml.Element ((name, attributes), children) ->
            (match t.jid with
            | Some jid ->
              let xml =
                Xml.remove_prefixes
                  (match from with
                  | None ->
                    let rec modify_from = function
                      | [] -> ["", Xml.From (Bare_JID (Jid.Full.to_bare jid))]
                      | (ns, Xml.From _) :: attrs ->
                        (ns, Xml.From (Bare_JID (Jid.Full.to_bare jid))) :: attrs
                      | a :: attrs -> a :: modify_from attrs
                    in
                    Xml.Element ((name, modify_from attributes), children)
                  | Some _ -> Xml.Element ((name, attributes), children))
              in
              if ato = Bare_JID (Jid.Full.to_bare jid)
              then t.callback (Some (Xml.to_string xml)) |> Lwt.return
              else (
                match%lwt
                  Rosters.get_subscription (Jid.to_bare_raw ato) (Jid.Full.to_bare jid)
                with
                | Some None | Some From ->
                  (match%lwt
                     Rosters.get_ask (Jid.to_bare_raw ato) (Jid.Full.to_bare jid)
                   with
                  | Some _ ->
                    let%lwt () =
                      Rosters.upgrade_subscription_to
                        (Jid.to_bare_raw ato)
                        (Jid.Full.to_bare jid)
                    in
                    let%lwt () =
                      Rosters.unset_ask (Jid.to_bare_raw ato) (Jid.Full.to_bare jid)
                    in
                    let%lwt connected_resources =
                      Connections.find_all (Jid.to_bare_raw ato)
                    in
                    connected_resources
                    |> Lwt_list.iter_s (fun (full_jid, handler) ->
                           handler
                             (Some
                                (SUBSCRIPTION_APPROVAL
                                   { ato
                                   ; xml
                                   ; from = Some (Bare_JID (Jid.Full.to_bare full_jid))
                                   }));
                           handler
                             (Some
                                (PUSH_ROSTER
                                   { ato = Some (Full_JID jid)
                                   ; contact = Bare_JID (Jid.Full.to_bare jid) }))
                           |> Lwt.return )
                  | None -> Lwt.return_unit)
                | _ -> Lwt.return_unit )
            | None -> Lwt.return_unit)
          | Xml.Text _ -> assert false)
        | SUBSCRIPTION_CANCELLATION {user; force} ->
          (match t.jid with
          | Some contact ->
            let aux () =
              let%lwt () =
                match%lwt
                  Rosters.get_item (Jid.to_bare_raw user) (Jid.Full.to_bare contact)
                with
                | Some _ ->
                  let%lwt connected_contact_resources =
                    Connections.find_all (Jid.Full.to_bare contact)
                  in
                  let%lwt connected_user_resources =
                    Connections.find_all (Jid.to_bare_raw user)
                  in
                  let%lwt () =
                    connected_contact_resources
                    |> Lwt_list.iter_s (fun (full_jid, _) ->
                           connected_user_resources
                           |> Lwt_list.iter_s (fun (_, handler) ->
                                  handler
                                    (Some
                                       (SEND_PRESENCE_UPDATE
                                          { from = Full_JID full_jid
                                          ; xml =
                                              Some
                                                ( Stanza.to_xml
                                                @@ Stanza.create_presence
                                                     ~id:(Some (Stanza.gen_id ()))
                                                     ~from:(Full_JID full_jid)
                                                     ~atype:"unavailable"
                                                     [] ) }))
                                  |> Lwt.return ) )
                  in
                  let%lwt () =
                    connected_user_resources
                    |> Lwt_list.iter_s (fun (full_jid, handler) ->
                           handler
                             (Some
                                (SEND_PRESENCE_UPDATE
                                   { from = Bare_JID (Jid.Full.to_bare full_jid)
                                   ; xml =
                                       Some
                                         ( Stanza.to_xml
                                         @@ Stanza.create_presence
                                              ~id:(Some (Stanza.gen_id ()))
                                              ~from:
                                                (Bare_JID (Jid.Full.to_bare full_jid))
                                              ~ato:(Bare_JID (Jid.Full.to_bare full_jid))
                                              ~atype:"unsubscribed"
                                              [] ) }))
                           |> Lwt.return )
                  in
                  let%lwt () =
                    Rosters.downgrade_subscription_to
                      (Jid.to_bare_raw user)
                      (Jid.Full.to_bare contact)
                  in
                  connected_user_resources
                  |> Lwt_list.iter_s (fun (full_jid, handler) ->
                         handler
                           (Some
                              (PUSH_ROSTER
                                 { ato = Some (Full_JID full_jid)
                                 ; contact = Full_JID contact }))
                         |> Lwt.return )
                | None -> Lwt.return_unit
              in
              let%lwt () =
                Rosters.downgrade_subscription_from
                  (Jid.Full.to_bare contact)
                  (Jid.to_bare_raw user)
              in
              t.actions_push (Some (PUSH_ROSTER {ato = None; contact = user}))
              |> Lwt.return
            in
            if force
            then aux ()
            else (
              match%lwt
                Rosters.get_subscription
                  (Jid.Full.to_bare contact)
                  (Jid.to_bare_raw user)
              with
              | Some From | Some Both -> aux ()
              | _ -> Lwt.return_unit )
          | None -> Lwt.return_unit)
        | UPDATE_PRESENCE {status; xml} ->
          (match t.jid with
          | Some jid ->
            let%lwt () = Rosters.set_presence (Jid.Full.to_bare jid) status in
            let%lwt subscribers = Rosters.get_subscribers (Jid.Full.to_bare jid) in
            (* Send presence updates to our subscribers and our own bare jid to ensure sync between resources *)
            [Jid.Full.to_bare jid] @ subscribers
            |> Lwt_list.iter_s (fun user ->
                   let%lwt connected_resources = Connections.find_all user in
                   Lwt_list.iter_s
                     (fun (contact, handler) ->
                       if contact <> jid
                       then
                         handler (Some (SEND_PRESENCE_UPDATE {from = Full_JID jid; xml}));
                       Lwt.return_unit )
                     connected_resources )
          | None -> Lwt.return_unit)
        | SEND_PRESENCE_UPDATE {from; xml} ->
          (match t.jid with
          | Some jid ->
            let rec modify_from = function
              | [] -> ["", Xml.From from]
              | (ns, Xml.From _) :: attrs -> (ns, Xml.From from) :: attrs
              | a :: attrs -> a :: modify_from attrs
            in
            let rec modify_to = function
              | [] -> ["", Xml.To (Bare_JID (Jid.Full.to_bare jid))]
              | (ns, Xml.To _) :: attrs ->
                (ns, Xml.To (Bare_JID (Jid.Full.to_bare jid))) :: attrs
              | a :: attrs -> a :: modify_to attrs
            in
            let%lwt stanza =
              match xml with
              | Some (Xml.Element ((name, attributes), children)) ->
                Stanza.Presence
                  ( Xml.Element ((name, modify_to @@ modify_from attributes), children)
                  |> Xml.remove_prefixes )
                |> Lwt.return
              | Some (Xml.Text _) -> assert false
              | None ->
                let partial_presence_stanza =
                  Stanza.create_presence
                    ~id:(Some (Stanza.gen_id ()))
                    ~from
                    ~ato:(Full_JID jid)
                in
                (match%lwt Rosters.get_presence (Jid.to_bare_raw from) with
                | Online -> Lwt.return (partial_presence_stanza [])
                | Offline -> Lwt.return (partial_presence_stanza ~atype:"unavailable" []))
            in
            t.callback (Some (Stanza.to_string stanza)) |> Lwt.return
          | None -> Lwt.return_unit)
        | SEND_CURRENT_PRESENCE ato ->
          (match t.jid with
          | Some jid ->
            let%lwt connected_user_resources =
              Connections.find_all (Jid.Full.to_bare jid)
            in
            let%lwt connected_contact_resources =
              Connections.find_all (Jid.to_bare_raw ato)
            in
            connected_contact_resources
            |> Lwt_list.iter_s (fun (_, handler) ->
                   connected_user_resources
                   |> Lwt_list.iter_s (fun (full_jid, _) ->
                          handler
                            (Some
                               (SEND_PRESENCE_UPDATE
                                  {from = Full_JID full_jid; xml = None}))
                          |> Lwt.return ) )
          | None -> Lwt.return_unit)
        | PROBE_PRESENCE ->
          (match t.jid with
          | Some jid ->
            let%lwt connected_resources_jids =
              let%lwt connected_resources =
                Connections.find_all (Jid.Full.to_bare jid)
              in
              connected_resources |> Lwt_list.map_s (fun (jid, _) -> Lwt.return jid)
            in
            let%lwt connected_subscriptions =
              let%lwt subscriptions = Rosters.get_subscriptions (Jid.Full.to_bare jid) in
              subscriptions
              |> Lwt_list.fold_left_s
                   (fun l contact ->
                     let%lwt connected_contact_resources =
                       Connections.find_all contact
                     in
                     let%lwt connected_contact_jids =
                       connected_contact_resources
                       |> Lwt_list.map_s (fun (j, _) -> Lwt.return j)
                     in
                     l @ connected_contact_jids |> Lwt.return )
                   []
            in
            connected_resources_jids @ connected_subscriptions
            |> Lwt_list.iter_s (fun full_jid ->
                   if full_jid <> jid
                   then
                     t.actions_push
                       (Some
                          (SEND_PRESENCE_UPDATE {from = Full_JID full_jid; xml = None}))
                     |> Lwt.return
                   else Lwt.return_unit )
          | None -> Lwt.return_unit)
        | IQ_ERROR {error_type; error_tag; id} ->
          let partial_iq_error =
            Stanza.create_iq_error
              ~from:(Jid.of_string t.hostname)
              ~id
              ~error_type
              ~error_tag
          in
          (match t.jid with
          | Some jid ->
            let iq_error = partial_iq_error ~ato:(Full_JID jid) () in
            let xml_string = Stanza.to_string iq_error in
            t.callback (Some xml_string) |> Lwt.return
          | None ->
            let iq_error = partial_iq_error () in
            let xml_string = Stanza.to_string iq_error in
            t.callback (Some xml_string) |> Lwt.return)
        | MESSAGE {ato; message} ->
          (match message with
          | Xml.Element ((name, attributes), children) as message ->
            (match t.jid with
            | Some jid ->
              if Jid.to_bare_raw ato = Jid.Full.to_bare jid
              then t.callback (Some (Xml.to_string message)) |> Lwt.return
              else
                let message =
                  Xml.Element
                    ((name, ("", Xml.From (Full_JID jid)) :: attributes), children)
                in
                let%lwt connected_resources =
                  Connections.find_all (Jid.to_bare_raw ato)
                in
                connected_resources
                |> Lwt_list.iter_s (fun (_, handler) ->
                       handler (Some (MESSAGE {ato; message})) |> Lwt.return )
            | None -> Lwt.return_unit)
          | Xml.Text _ -> assert false)
        | SUBSCRIPTION_REMOVAL {contact} ->
          (match t.jid with
          | Some jid ->
            let user = Jid.Full.to_bare jid in
            let%lwt connected_user_resources = Connections.find_all user in
            let%lwt () =
              match%lwt Rosters.get_subscription (Jid.to_bare_raw contact) user with
              | Some From | Some Both ->
                (* deliver unsubscribe stanza to all contacts resources *)
                let%lwt connected_contact_resources =
                  Connections.find_all (Jid.to_bare_raw contact)
                in
                let%lwt () =
                  connected_contact_resources
                  |> Lwt_list.iter_s (fun (full_jid, handler) ->
                         handler
                           (Some
                              (SEND_PRESENCE_UPDATE
                                 { from = Bare_JID user
                                 ; xml =
                                     Some
                                       ( Stanza.to_xml
                                       @@ Stanza.create_presence
                                            ~id:(Some (Stanza.gen_id ()))
                                            ~from:(Bare_JID user)
                                            ~ato:(Full_JID full_jid)
                                            ~atype:"unsubscribe"
                                            [] ) }))
                         |> Lwt.return )
                in
                (* roster push to contacts resources with sub=none / to *)
                let%lwt () =
                  Rosters.downgrade_subscription_from (Jid.to_bare_raw contact) user
                in
                let%lwt () =
                  connected_contact_resources
                  |> Lwt_list.iter_s (fun (full_jid, handler) ->
                         handler
                           (Some
                              (PUSH_ROSTER
                                 {ato = Some (Full_JID full_jid); contact = Bare_JID user}))
                         |> Lwt.return )
                in
                (* unavailable from contacts resources to bare user *)
                connected_contact_resources
                |> Lwt_list.iter_s (fun (full_jid, _) ->
                       connected_user_resources
                       |> Lwt_list.iter_s (fun (_, handler) ->
                              handler
                                (Some
                                   (SEND_PRESENCE_UPDATE
                                      { from = Full_JID full_jid
                                      ; xml =
                                          Some
                                            ( Stanza.to_xml
                                            @@ Stanza.create_presence
                                                 ~id:(Some (Stanza.gen_id ()))
                                                 ~from:(Full_JID full_jid)
                                                 ~ato:(Bare_JID user)
                                                 ~atype:"unavailable"
                                                 [] ) }))
                              |> Lwt.return ) )
              | _ -> Lwt.return_unit
            in
            (* Roster push to users resources with none/from *)
            let%lwt () =
              Rosters.downgrade_subscription_to user (Jid.to_bare_raw contact)
            in
            connected_user_resources
            |> Lwt_list.iter_s (fun (full_jid, handler) ->
                   handler (Some (PUSH_ROSTER {ato = Some (Full_JID full_jid); contact}))
                   |> Lwt.return )
          | None -> Lwt.return_unit)
        | CLOSE ->
          (* After closing the stream we aren't allowed to send anything more so stop handling any more actions *)
          t.callback (Some "</stream:stream>");
          (t.closed <- true) |> Lwt.return
        | ERROR e ->
          t.callback (Some e);
          (t.closed <- true) |> Lwt.return
      in
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
  let user = None in
  let jid = None in
  let fsm = State.initial in
  let actions_stream, actions_push = Lwt_stream.create () in
  let t = {parser; callback; user; jid; fsm; actions_push; hostname; closed = false} in
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

let to_string t = Sexplib.Sexp.to_string_hum @@ sexp_of_t t

let make_test_handler s =
  let%lwt () = Rosters.clear () in
  let%lwt () = Connections.clear () in
  let stream = Lwt_stream.of_string s in
  let callback so =
    match so with
    | Some s -> print_endline (Utils.mask_id s)
    | None -> print_endline "Out stream closed"
  in
  create ~stream ~callback ~hostname:"im.example.com" |> Lwt.return
;;

let test_stanza stanza =
  let run =
    let%lwt handler = make_test_handler stanza in
    let%lwt () = handle handler in
    Lwt.return handler
  in
  Lwt_main.run run
;;

let%expect_test "initial stanza with version" =
  let stanza =
    Stream.to_string
      (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
    <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
    <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism><mechanism>ANONYMOUS</mechanism></mechanisms></stream:features>
    Unexpected stream close during sasl negotiation
    Out stream closed
    |}];
  print_endline (to_string handler);
  [%expect
    {|
    ((parser ((raw_stream <opaque>) (stream <opaque>) (depth 1)))
     (callback <fun>) (user ()) (hostname im.example.com) (jid ())
     (fsm ((state CLOSED))) (actions_push <fun>) (closed true)) |}]
;;

let%expect_test "error in initial stanza" =
  let stanza =
    Stream.to_string
      (Header (Stream.create_header ~ato:(Jid.of_string "im.example.com") ()))
    ^ "</stream:stream>"
  in
  let handler = test_stanza stanza in
  [%expect
    {|
    <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
    <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism><mechanism>ANONYMOUS</mechanism></mechanisms></stream:features>
    Unexpected stream close during sasl negotiation
    Out stream closed |}];
  print_endline (to_string handler);
  [%expect
    {|
    ((parser ((raw_stream <opaque>) (stream <opaque>) (depth 1)))
     (callback <fun>) (user ()) (hostname im.example.com) (jid ())
     (fsm ((state CLOSED))) (actions_push <fun>) (closed true)) |}]
;;

let%expect_test "bind resource" =
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
    <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism><mechanism>ANONYMOUS</mechanism></mechanisms></stream:features>
    <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>
    <stream:stream id='<redacted_for_testing>' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' from='im.example.com'>
    <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
    <iq id='<redacted_for_testing>' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
    </stream:stream>
    Out stream closed |}];
  print_endline (to_string handler);
  [%expect
    {|
    ((parser ((raw_stream <opaque>) (stream <opaque>) (depth 1)))
     (callback <fun>) (user (juliet)) (hostname im.example.com)
     (jid (((juliet im.example.com) balcony))) (fsm ((state CLOSED)))
     (actions_push <fun>) (closed true)) |}]
;;

let%expect_test "roster get" =
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
      <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism><mechanism>ANONYMOUS</mechanism></mechanisms></stream:features>
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
      ((parser ((raw_stream <opaque>) (stream <opaque>) (depth 1)))
       (callback <fun>) (user (juliet)) (hostname im.example.com)
       (jid (((juliet im.example.com) balcony))) (fsm ((state CLOSED)))
       (actions_push <fun>) (closed true))
     |}]
;;

let%expect_test "roster set" =
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
      <stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism><mechanism>ANONYMOUS</mechanism></mechanisms></stream:features>
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
      ((parser ((raw_stream <opaque>) (stream <opaque>) (depth 1)))
       (callback <fun>) (user (juliet)) (hostname im.example.com)
       (jid (((juliet im.example.com) balcony))) (fsm ((state CLOSED)))
       (actions_push <fun>) (closed true))
     |}]
;;
