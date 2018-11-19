type t =
  | STANZA of Stanza.t
  | CLOSE

let to_string = function STANZA s -> "stanza" ^ Stanza.to_string s | CLOSE -> "close"
