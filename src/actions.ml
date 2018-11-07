type t =
  | SEND_STANZA of Stanza.t
  | ROSTER_UPDATE
  | ROSTER_GET
  | PASS_STANZA

let to_string = function
  | SEND_STANZA _ -> "send_xml"
  | ROSTER_GET -> "roster_get"
  | ROSTER_UPDATE -> "roster_update"
  | PASS_STANZA -> "pass_stanza"
;;
