type t =
  | SEND_STREAM_HEADER of Xml.tag
  | SEND_STREAM_FEATURES of Xml.t
  | REPLY_STANZA of Stanza.t
  | SEND_STANZA of Jid.t * Stanza.t
  | CLOSE
  | ERROR of string
  | SET_JID of Jid.t
  | SET_JID_RESOURCE of string * string
  | GET_ROSTER of string * string

let to_string = function
  | SEND_STREAM_HEADER tag ->
    "SEND_STREAM_HEADER: " ^ Utils.mask_id (Xml.tag_to_string ~empty:true tag)
  | SEND_STREAM_FEATURES xml -> "SEND_STREAM_FEATURES: " ^ Xml.to_string xml
  | REPLY_STANZA s -> "REPLY_STANZA: " ^ Utils.mask_id (Stanza.to_string s)
  | SEND_STANZA (jid, s) ->
    "SEND_STANZA:\n" ^ Jid.to_string jid ^ "\n" ^ Stanza.to_string s
  | CLOSE -> "CLOSE"
  | ERROR s -> "ERROR: " ^ s
  | SET_JID j -> "SET_JID: " ^ Jid.to_string j
  | SET_JID_RESOURCE (id, res) -> "SET_JID_RESOURCE: id=" ^ id ^ " res=" ^ res
  | GET_ROSTER (from, id) -> "GET_ROSTER: from=" ^ from ^ " id=" ^ id
;;
