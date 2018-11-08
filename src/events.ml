type t = STANZA of Stanza.t

let to_string = function STANZA s -> "stanza" ^ Stanza.to_string s
