type t =
  | STREAM_HEADER of Xml.tag
  | RESOURCE_BIND_SERVER_GEN of string
  | RESOURCE_BIND_CLIENT_GEN of string * string
  | STREAM_CLOSE
  | ERROR of string
  | ROSTER_GET of string * string

let to_string = function
  | STREAM_HEADER tag -> "STREAM_HEADER: " ^ Xml.tag_to_string ~empty:true tag
  | RESOURCE_BIND_SERVER_GEN _id -> "RESOURCE_BIND_SERVER_GEN: id"
  | RESOURCE_BIND_CLIENT_GEN (id, jid) ->
    "RESOURCE_BIND_CLIENT_GEN: id=" ^ id ^ " jid=" ^ jid
  | STREAM_CLOSE -> "STREAM_CLOSE"
  | ERROR s -> "ERROR: " ^ s
  | ROSTER_GET (from, id) -> "ROSTER_GET: from=" ^ from ^ " id=" ^ id
;;

let not_implemented = ERROR "not implemented"

let lift_iq = function
  | Xml.Element (((_prefix, _name), attributes), children) ->
    (match Stanza.get_value (Stanza.get_attribute_by_name_exn attributes "type") with
    | "set" ->
      (match children with
      | [Xml.Element (((_p, "bind"), _attrs), [])] ->
        (* resource bind with server-generated resource identifier (7.6) *)
        RESOURCE_BIND_SERVER_GEN (Stanza.get_id attributes)
      | [Xml.Element (((_p, "bind"), _attrs), [child])] ->
        (match child with
        | Xml.Element (((_, "resource"), []), [Xml.Text t]) ->
          RESOURCE_BIND_CLIENT_GEN (Stanza.get_id attributes, t)
        | _ -> not_implemented)
      | _ -> not_implemented)
    | "get" ->
      (match children with
      | [Xml.Element (((_p, "query"), _attrs), [])] ->
        (* roster get query *)
        ROSTER_GET (Stanza.get_from attributes, Stanza.get_id attributes)
      | _ -> not_implemented)
    | _ -> not_implemented)
  | Xml.Text _t -> not_implemented
;;

let lift parse_result =
  let open Parser in
  match parse_result with
  | Stanza stanza ->
    (match stanza with
    | Stanza.Iq element -> lift_iq element
    | Stanza.Presence (Element (((_prefix, _name), _attributes), _children) as _xml) ->
      not_implemented
    | Stanza.Message (Element (((_prefix, _name), _attributes), _children) as _xml) ->
      not_implemented
    | _ -> ERROR "Not expecing text elements")
  | Stream_Element stream_element ->
    (match stream_element with
    | Header tag -> STREAM_HEADER tag
    | Features -> not_implemented
    | Error -> not_implemented
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
                 , [ ("", "from"), "juliet@capulet.com/balcony"
                   ; ("", "id"), "h83vxa4c"
                   ; ("", "type"), "get" ] )
               , [Xml.Element ((("", "query"), []), [])] ))))
  in
  print_endline (to_string event);
  [%expect {| ROSTER_GET: from=juliet@capulet.com/balcony id=h83vxa4c |}]
;;

let%expect_test "iq set" =
  let event =
    lift
      (Stanza
         (Stanza.Iq
            (Element
               ( (("", "iq"), [("", "id"), "l3b1vs75"; ("", "type"), "set"])
               , [Xml.Element ((("", "bind"), []), [])] ))))
  in
  print_endline (to_string event);
  [%expect {| RESOURCE_BIND_SERVER_GEN: id |}]
;;
