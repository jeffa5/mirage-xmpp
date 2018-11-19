type t =
  | REPLY_STANZA of bool * Stanza.t
  | SEND_STANZA of Jid.t * Stanza.t
  | CLOSE

let to_string = function
  | REPLY_STANZA (b, s) -> "reply_stanza:\n" ^ Stanza.pp_to_string ~auto_close:b s
  | SEND_STANZA (jid, s) ->
    "send_stanza:\n" ^ Jid.to_string jid ^ "\n" ^ Stanza.pp_to_string s
  | CLOSE -> "close"
;;
