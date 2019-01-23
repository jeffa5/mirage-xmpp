open Astring
open Sexplib.Std

type t =
  | STREAM_HEADER of {version : string}
  | SASL_AUTH of {user : string; password : string}
  | RESOURCE_BIND_SERVER_GEN of string
  | RESOURCE_BIND_CLIENT_GEN of {id : string; resource : string}
  | SESSION_START of string
  | STREAM_CLOSE
  | ERROR of string
  | ROSTER_GET of string
  | ROSTER_SET of {id : string; target : Jid.t; handle : string; groups : string list}
  | SUBSCRIPTION_REQUEST of {ato : Jid.t; xml : Xml.t}
  | PRESENCE_UPDATE of {status : Rosters.presence; xml : Xml.t option}
  | IQ_ERROR of {error_type : Actions.error_type; error_tag : string; id : string}
  | MESSAGE of {ato : Jid.t; message : Xml.t}
  | LOG_OUT
  | NOOP
  | ROSTER_REMOVE of {id : string; target : Jid.t}
  | SUBSCRIPTION_APPROVAL of {ato : Jid.t; xml : Xml.t}
  | SUBSCRIPTION_CANCELLATION of {user : Jid.t}
  | SUBSCRIPTION_REMOVAL of {contact : Jid.t}
[@@deriving sexp]

let to_string t = Sexplib.Sexp.to_string_hum @@ sexp_of_t t
let not_implemented = ERROR "not implemented"

let lift_iq = function
  | Xml.Element (((_prefix, _name), attributes), children) ->
    (match Stanza.get_type attributes with
    | Some "set" ->
      (match children with
      | [Xml.Element (((_p, "bind"), _attrs), [])] ->
        (* resource bind with server-generated resource identifier (7.6) *)
        RESOURCE_BIND_SERVER_GEN (Stanza.get_id_exn attributes)
      | [Xml.Element (((_p, "bind"), _attrs), [child])] ->
        (match child with
        | Xml.Element (((_, "resource"), []), [Xml.Text resource]) ->
          RESOURCE_BIND_CLIENT_GEN {id = Stanza.get_id_exn attributes; resource}
        | _ -> ERROR "Unexpected child of resource bind")
      | [ Xml.Element
            ( ((_, "query"), [(_, Xml.Xmlns "jabber:iq:roster")])
            , [Xml.Element (((_, "item"), attrs), group_elements)] ) ] ->
        (match Stanza.get_subscription attrs with
        | Some "remove" ->
          ROSTER_REMOVE {id = Stanza.get_id_exn attributes; target = Stanza.get_jid attrs}
        | _ ->
          let groups =
            List.map
              (fun element ->
                match element with
                | Xml.Element (((_, "group"), _), [Xml.Text group]) -> group
                | _ -> assert false )
              group_elements
          in
          let jid = Stanza.get_jid attrs in
          let handle =
            match Stanza.get_name attrs with Some name -> name | None -> ""
          in
          ROSTER_SET {id = Stanza.get_id_exn attributes; target = jid; handle; groups})
      | [Xml.Element (((_, "session"), _), [])] ->
        SESSION_START (Stanza.get_id_exn attributes)
      | _ ->
        ERROR
          ( "No children matched for iq of type set\n"
          ^ String.concat ~sep:"\nnext xml: "
          @@ List.map
               (function
                 | Xml.Element _ as element -> "Element: " ^ Xml.to_string element
                 | Xml.Text _ as text -> "Text: " ^ Xml.to_string text)
               children ))
    | Some "get" ->
      (match children with
      | [Xml.Element (((_, "query"), [(_, Xml.Xmlns "jabber:iq:roster")]), _)] ->
        (* roster get query *)
        ROSTER_GET (Stanza.get_id_exn attributes)
      | _ ->
        let id = Stanza.get_id_exn attributes in
        IQ_ERROR {error_type = Actions.Cancel; error_tag = "feature-not-implemented"; id})
    | Some "result" -> NOOP
    | _ -> ERROR "Type of iq expected to be 'set' or 'get'")
  | Xml.Text _t -> ERROR "Expected an iq stanza, not text"
;;

let lift_presence = function
  | Xml.Element (((namespace, name), attributes), children) ->
    (match Stanza.get_type attributes with
    | Some "subscribe" ->
      let rec modify_to = function
        | [] -> []
        | (ns, Xml.To jid) :: attrs -> (ns, Xml.To (Jid.to_bare jid)) :: attrs
        | a :: attrs -> a :: modify_to attrs
      in
      let ato = Stanza.get_to attributes |> Jid.to_bare in
      SUBSCRIPTION_REQUEST
        {ato; xml = Xml.Element (((namespace, name), modify_to attributes), children)}
    | Some "subscribed" ->
      let rec modify_to = function
        | [] -> []
        | (ns, Xml.To jid) :: attrs -> (ns, Xml.To (Jid.to_bare jid)) :: attrs
        | a :: attrs -> a :: modify_to attrs
      in
      let ato = Stanza.get_to attributes |> Jid.to_bare in
      SUBSCRIPTION_APPROVAL
        {ato; xml = Xml.Element (((namespace, name), modify_to attributes), children)}
    | Some "unavailable" ->
      PRESENCE_UPDATE
        { status = Rosters.Offline
        ; xml = Some (Xml.Element (((namespace, name), attributes), children)) }
    | Some "unsubscribed" ->
      SUBSCRIPTION_CANCELLATION {user = Stanza.get_to attributes |> Jid.to_bare}
    | Some "unsubscribe" ->
      SUBSCRIPTION_REMOVAL {contact = Stanza.get_to attributes |> Jid.to_bare}
    | None ->
      PRESENCE_UPDATE
        { status = Rosters.Online
        ; xml = Some (Xml.Element (((namespace, name), attributes), children)) }
    | _ -> not_implemented)
  | Xml.Text _t -> ERROR "Expected a presence stanza, not text"
;;

let lift_message = function
  | Xml.Element (((_prefix, _name), attributes), _children) as message ->
    let ato = Stanza.get_to attributes in
    let message = Xml.remove_prefixes message in
    MESSAGE {ato; message}
  | Xml.Text _t -> ERROR "Expected a message stanza, not text"
;;

let lift parse_result =
  let open Parser in
  match parse_result with
  | Stanza stanza ->
    (match stanza with
    | Stanza.Iq element -> lift_iq element
    | Stanza.Presence element -> lift_presence element
    | Stanza.Message element -> lift_message element)
  | Sasl_auth xml ->
    let rec get_mechanism = function
      | [] -> raise Not_found
      | (_, Xml.Mechanism mechanism) :: _ -> mechanism
      | _ :: attrs -> get_mechanism attrs
    in
    (match xml with
    | Element ((_name, attributes), [Text b64_string]) ->
      if get_mechanism attributes = "PLAIN"
      then
        let decoded_string = B64.decode b64_string in
        match String.cut ~sep:"\000" (String.trim decoded_string) with
        | Some (_userdom, userpass) ->
          (match String.cut ~sep:"\000" userpass with
          | Some (user, pass) -> SASL_AUTH {user; password = pass}
          | None -> ERROR "SASL: couldn't find second 0 byte")
        | _ -> ERROR "SASL: couldn't find first 0 byte"
      else
        ERROR
          ( Xml.to_string
          @@ Xml.create
               ~children:[Xml.create (("", "invalid-mechanism"), [])]
               (("", "failure"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-sasl"]) )
    | _ ->
      ERROR
        ( Xml.to_string
        @@ Xml.create
             ~children:[Xml.create (("", "invalid-mechanism"), [])]
             (("", "failure"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-sasl"]) ))
  | Stream_Element stream_element ->
    (match stream_element with
    | Header (_name, attributes) ->
      let version = Stanza.get_version attributes in
      STREAM_HEADER {version}
    | Features -> not_implemented
    | Error -> ERROR "Stream level error"
    | Close -> STREAM_CLOSE)
  | Error e -> ERROR e
;;

let%expect_test "lift error gives error" =
  let event = lift (Error "some error") in
  print_endline (to_string event);
  [%expect {| (ERROR "some error") |}]
;;

let%expect_test "iq get" =
  let event =
    lift
      (Stanza
         (Stanza.Iq
            (Element
               ( ( ("", "iq")
                 , [ "", Xml.From (Jid.of_string "juliet@capulet.com/balcony")
                   ; "", Xml.Id "h83vxa4c"
                   ; "", Xml.Type "get" ] )
               , [Xml.Element ((("", "query"), ["", Xml.Xmlns "jabber:iq:roster"]), [])]
               ))))
  in
  print_endline (to_string event);
  [%expect {| (ROSTER_GET h83vxa4c) |}]
;;

let%expect_test "iq set" =
  let event =
    lift
      (Stanza
         (Stanza.Iq
            (Element
               ( (("", "iq"), ["", Xml.Id "l3b1vs75"; "", Xml.Type "set"])
               , [Xml.Element ((("", "bind"), []), [])] ))))
  in
  print_endline (to_string event);
  [%expect {| (RESOURCE_BIND_SERVER_GEN l3b1vs75) |}]
;;

let%expect_test "roster get" =
  let event =
    lift
      (Stanza
         (Stanza.Iq
            (Element
               ( ( ("", "iq")
                 , [ "", Xml.From (Jid.of_string "juliet@example.com/balony")
                   ; "", Xml.Id "bv1bs71f"
                   ; "", Xml.Type "get" ] )
               , [Xml.Element ((("", "query"), ["", Xml.Xmlns "jabber:iq:roster"]), [])]
               ))))
  in
  print_endline (to_string event);
  [%expect {| (ROSTER_GET bv1bs71f) |}]
;;

let%expect_test "roster set" =
  let event =
    lift
      (Stanza
         (Stanza.Iq
            (Element
               ( ( ("", "iq")
                 , [ "", Xml.From (Jid.of_string "juliet@example.com/balony")
                   ; "", Xml.Id "rs1"
                   ; "", Xml.Type "set" ] )
               , [ Xml.Element
                     ( (("", "query"), ["", Xml.Xmlns "jabber:iq:roster"])
                     , [ Xml.Element
                           ( ( ("", "item")
                             , [ "", Xml.Jid (Jid.of_string "nurse@example.com")
                               ; "", Xml.Name "Nurse" ] )
                           , [] ) ] ) ] ))))
  in
  print_endline (to_string event);
  [%expect
    {|
    (ROSTER_SET (id rs1) (target (Bare_JID (nurse example.com))) (handle Nurse)
     (groups ())) |}]
;;
