type t =
  | INITIAL of Stanza.t
  | STARTING
  | CLOSING

let lift _ = STARTING

let to_string = function
  | STARTING -> "starting"
  | CLOSING -> "closing"
  | INITIAL s -> "initial" ^ Stanza.to_string s
;;
