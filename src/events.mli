(** The module to handle conversion of stanzas into events for the state machine *)

(** The type of events, examples for now *)
type t =
  | STREAM_HEADER of {from : Jid.t; ato : Jid.t; version : string}
  | RESOURCE_BIND_SERVER_GEN of string
  | RESOURCE_BIND_CLIENT_GEN of string * string
  | STREAM_CLOSE
  | ERROR of string
  | ROSTER_GET of string * Jid.t
  | ROSTER_SET of string * Jid.t * Jid.t * string * string * string list

(** [to_string t] takes an event and returns it's string representation *)
val to_string : t -> string

(** [lift pr] converts the parse_result [pr] into an event type suitable for sending to the state machine *)
val lift : Parser.parse_result -> t
