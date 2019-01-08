open Astring

type t =
  | STREAM_HEADER of {ato : Jid.t; version : string}
  | SASL_AUTH of {user : string; password : string}
  | RESOURCE_BIND_SERVER_GEN of string
  | RESOURCE_BIND_CLIENT_GEN of {id : string; resource : string}
  | SESSION_START of string
  | STREAM_CLOSE
  | ERROR of string
  | ROSTER_GET of string
  | ROSTER_SET of
      { id : string
      ; target : Jid.t
      ; handle : string
      ; subscription : Rosters.subscription
      ; groups : string list }
  | SUBSCRIPTION_REQUEST of {id : string; ato : Jid.t}
  | PRESENCE_UPDATE of Rosters.availability
  | IQ_ERROR of {error_type : Actions.error_type; error_tag : string; id : string}
  | MESSAGE of {ato : Jid.t; message : Xml.t}
  | LOG_OUT

let to_string = function
  | STREAM_HEADER {ato; version} ->
    "STREAM_HEADER: to=" ^ Jid.to_string ato ^ " version=" ^ version
  | SASL_AUTH {user; password} -> "SASL_AUTH: user=" ^ user ^ " password=" ^ password
  | RESOURCE_BIND_SERVER_GEN _id -> "RESOURCE_BIND_SERVER_GEN: id"
  | RESOURCE_BIND_CLIENT_GEN {id; resource} ->
    "RESOURCE_BIND_CLIENT_GEN: id=" ^ id ^ " resource=" ^ resource
  | SESSION_START id -> "SESSION_START: id=" ^ id
  | STREAM_CLOSE -> "STREAM_CLOSE"
  | ERROR s -> "ERROR: " ^ s
  | ROSTER_GET id -> "ROSTER_GET: id=" ^ id
  | ROSTER_SET {id; target; handle; subscription; groups} ->
    "ROSTER_SET: id="
    ^ id
    ^ " target="
    ^ Jid.to_string target
    ^ " handle="
    ^ handle
    ^ " subscribed="
    ^ Rosters.subscription_to_string subscription
    ^ " groups=["
    ^ String.concat ~sep:" " groups
    ^ "]"
  | SUBSCRIPTION_REQUEST {id; ato} ->
    "SUBSCRIPTION_REQUEST: id=" ^ id ^ " to=" ^ Jid.to_string ato
  | PRESENCE_UPDATE availability ->
    "PRESENCE_UPDATE: availability=" ^ Rosters.availability_to_string availability
  | IQ_ERROR {error_type; error_tag; id} ->
    "IQ_ERROR: error_type="
    ^ Actions.error_type_to_string error_type
    ^ " error_tag="
    ^ error_tag
    ^ " id="
    ^ id
  | MESSAGE {ato; message} ->
    "MESSAGE: to=" ^ Jid.to_string ato ^ " message=" ^ Xml.to_string message
  | LOG_OUT -> "LOG_OUT"
;;

let not_implemented = ERROR "not implemented"

let lift_iq = function
  | Xml.Element (((_prefix, _name), attributes), children) ->
    (match Stanza.get_type attributes with
    | Some "set" ->
      (match children with
      | [Xml.Element (((_p, "bind"), _attrs), [])] ->
        (* resource bind with server-generated resource identifier (7.6) *)
        RESOURCE_BIND_SERVER_GEN (Stanza.get_id attributes)
      | [Xml.Element (((_p, "bind"), _attrs), [child])] ->
        (match child with
        | Xml.Element (((_, "resource"), []), [Xml.Text resource]) ->
          RESOURCE_BIND_CLIENT_GEN {id = Stanza.get_id attributes; resource}
        | _ -> ERROR "Unexpected child of resource bind")
      | [ Xml.Element
            ( ((_, "query"), [(_, Xml.Xmlns "jabber:iq:roster")])
            , [Xml.Element (((_, "item"), attrs), group_elements)] ) ] ->
        let groups =
          List.map
            (fun element ->
              match element with
              | Xml.Element (((_, "group"), _), [Xml.Text group]) -> group
              | _ -> assert false )
            group_elements
        in
        let jid = Stanza.get_jid attrs in
        let handle = (match Stanza.get_name attrs with
            | Some name -> name
            | None -> "") in
        ROSTER_SET
          { id = Stanza.get_id attributes
          ; target = jid
          ; handle
          ; subscription = Rosters.None
          ; groups }
      | [Xml.Element (((_, "session"), _), [])] ->
        SESSION_START (Stanza.get_id attributes)
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
        ROSTER_GET (Stanza.get_id attributes)
      | _ ->
        let id = Stanza.get_id attributes in
        IQ_ERROR {error_type = Actions.Cancel; error_tag = "feature-not-implemented"; id})
    | _ -> ERROR "Type of iq expected to be 'set' or 'get'")
  | Xml.Text _t -> ERROR "Expected an iq stanza, not text"
;;

let lift_presence = function
  | Xml.Element (((_prefix, _name), attributes), _children) ->
    (match Stanza.get_type attributes with
    | Some "subscribe" ->
      SUBSCRIPTION_REQUEST {id = Stanza.get_id attributes; ato = Stanza.get_to attributes}
    | Some "unavailable" -> LOG_OUT
    | None -> PRESENCE_UPDATE Rosters.Online
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
      let ato = Stanza.get_to attributes in
      let version = Stanza.get_version attributes in
      STREAM_HEADER {ato; version}
    | Features -> not_implemented
    | Error -> ERROR "Stream level error"
    | Close -> STREAM_CLOSE)
  | Error e -> ERROR e
;;

let%expect_test "lift error gives error" =
  let event = lift (Error "some error") in
  print_endline (to_string event);
  [%expect {| ERROR: some error |}]
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
  [%expect {| ROSTER_GET: id=h83vxa4c |}]
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
  [%expect {| RESOURCE_BIND_SERVER_GEN: id |}]
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
  [%expect {| ROSTER_GET: id=bv1bs71f |}]
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
    {| ROSTER_SET: id=rs1 target=nurse@example.com handle=Nurse subscribed=none groups=[] |}]
;;
