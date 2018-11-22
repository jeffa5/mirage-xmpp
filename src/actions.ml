type t =
  | REPLY_STANZA of bool * Stanza.t
  | SEND_STANZA of Jid.t * Stanza.t
  | CLOSE
  | ERROR of string
  | SET_JID of Jid.t
  | SET_JID_RESOURCE of string * string

let to_string = function
  | REPLY_STANZA (b, s) -> "reply_stanza:\n" ^ Stanza.pp_to_string ~auto_close:b s
  | SEND_STANZA (jid, s) ->
    "send_stanza:\n" ^ Jid.to_string jid ^ "\n" ^ Stanza.pp_to_string s
  | CLOSE -> "close"
  | ERROR s -> "error: " ^ s
  | SET_JID j -> "set_jid: " ^ Jid.to_string j
  | SET_JID_RESOURCE (id, res) -> "set_jid_resource: id=" ^ id ^ " res=" ^ res
;;
