type t =
  | REPLY_STANZA of bool * Stanza.t
  | SEND_STANZA of Jid.t * Stanza.t
  | ROSTER_UPDATE
  | ROSTER_GET

let to_string = function
  | REPLY_STANZA (b, s) -> "reply_stanza:\n" ^ Stanza.pp_to_string ~auto_close:b s
  | SEND_STANZA (jid, s) ->
    "send_stanza:\n" ^ Jid.to_string jid ^ "\n" ^ Stanza.pp_to_string s
  | ROSTER_GET -> "roster_get"
  | ROSTER_UPDATE -> "roster_update"
;;
