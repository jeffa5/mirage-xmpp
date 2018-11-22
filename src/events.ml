type t =
  | STREAM_HEADER of Stanza.t
  | RESOURCE_BIND_SERVER_GEN of string
  | RESOURCE_BIND_CLIENT_GEN of string * string
  | CLOSE
  | ERROR of string

let to_string = function
  | STREAM_HEADER s -> "stream header: " ^ Stanza.pp_to_string s
  | RESOURCE_BIND_SERVER_GEN id -> "resource bind server gen: id=" ^ id
  | RESOURCE_BIND_CLIENT_GEN (id, jid) ->
    "resource bind client gen: id=" ^ id ^ " jid=" ^ jid
  | CLOSE -> "close"
  | ERROR s -> "error: " ^ s
;;

let not_implemented = ERROR "not implemented"

let lift_iq = function
  | Stanza.Stanza (((_prefix, _name), attributes), children) ->
    (match Stanza.get_value (Stanza.get_attribute_by_name_exn attributes "type") with
     | "set" ->
       (match children with
        | [Stanza.Stanza (((_p, "bind"), attrs), [])] ->
          (* resource bind with server-generated resource identifier (7.6) *)
          RESOURCE_BIND_SERVER_GEN (Stanza.get_id attrs)
        | [Stanza.Stanza (((_p, "bind"), _attrs), [child])] ->
          (match child with
           | Stanza.Stanza (((_, "resource"), []), [Stanza.Text [t]]) ->
             RESOURCE_BIND_CLIENT_GEN (Stanza.get_id attributes, t)
           | _ -> not_implemented)
        | _ -> not_implemented)
     | _ -> not_implemented)
  | Text _t -> not_implemented
;;

let lift parse_result =
  let open Parser in
  match parse_result with
  | Stanza stanza ->
    (match stanza with
     | Stanza.Stanza (((_prefix, name), attributes), _children) ->
       (match name with
        | "stream" ->
          (* This is a stream header *)
          (* check the stream namespace is correct *)
          if Stanza.get_value (Stanza.get_attribute_by_name_exn attributes "stream")
             = "http://etherx.jabber.org/streams"
          then STREAM_HEADER stanza
          else ERROR "invalid-namespace"
        | "iq" -> lift_iq stanza
        | _ -> not_implemented)
     | Stanza.Text t -> ERROR ("Expecting stanza with tag, not: " ^ String.concat " " t))
  | End -> CLOSE
  | Error e -> ERROR e
;;
