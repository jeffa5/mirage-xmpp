type t =
  | SEND_STREAM_HEADER of Xml.tag
  | SEND_STREAM_FEATURES of Xml.t
  | REPLY_STANZA of Stanza.t
  | SERVER_GEN_RESOURCE_IDENTIFIER of string
  | CLOSE
  | ERROR of string
  | SET_JID of Jid.t
  | SET_JID_RESOURCE of string * string
  | GET_ROSTER of string * Jid.t
  | SET_ROSTER of string * Jid.t * Jid.t * string * string * string list
  | PUSH_ROSTER of Jid.t * Jid.t
  | ADD_TO_CONNECTIONS
  | REMOVE_FROM_CONNECTIONS

let to_string = function
  | SEND_STREAM_HEADER tag ->
    "SEND_STREAM_HEADER: " ^ Utils.mask_id (Xml.tag_to_string ~empty:true tag)
  | SEND_STREAM_FEATURES xml -> "SEND_STREAM_FEATURES: " ^ Xml.to_string xml
  | REPLY_STANZA s -> "REPLY_STANZA: " ^ Utils.mask_id (Stanza.to_string s)
  | SERVER_GEN_RESOURCE_IDENTIFIER s -> "SERVER_GEN_RESOURCE_IDENTIFIER: " ^ s
  | CLOSE -> "CLOSE"
  | ERROR s -> "ERROR: " ^ s
  | SET_JID j -> "SET_JID: " ^ Jid.to_string j
  | SET_JID_RESOURCE (id, res) -> "SET_JID_RESOURCE: id=" ^ id ^ " res=" ^ res
  | GET_ROSTER (id, from) -> "GET_ROSTER: id=" ^ id ^ " from=" ^ Jid.to_string from
  | SET_ROSTER (id, from, target, handle, subscribed, groups) ->
    "SET_ROSTER: id="
    ^ id
    ^ " from="
    ^ Jid.to_string from
    ^ " target="
    ^ Jid.to_string target
    ^ " handle="
    ^ handle
    ^ " subscribed="
    ^ subscribed
    ^ " groups=["
    ^ String.concat " " groups
    ^ "]"
  | PUSH_ROSTER (bare_jid, updated_jid) ->
    "PUSH_ROSTER: bare_jid="
    ^ Jid.to_string bare_jid
    ^ " updated_jid="
    ^ Jid.to_string updated_jid
  | ADD_TO_CONNECTIONS -> "ADD_TO_CONNECTIONS"
  | REMOVE_FROM_CONNECTIONS -> "REMOVE_FROM_CONNECTIONS"
;;
