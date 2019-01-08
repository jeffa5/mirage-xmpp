(** The module to handle conversion of stanzas into events for the state machine *)

(** The type of events, examples for now *)
type t =
  | STREAM_HEADER of {ato : Jid.t; version : string}
  | SASL_AUTH of {user : string; password : string}
  | RESOURCE_BIND_SERVER_GEN of string
  | RESOURCE_BIND_CLIENT_GEN of {id : string; resource : string}
  | SESSION_START of string
  | STREAM_CLOSE
  | ERROR of string
  | ROSTER_GET of string
  | ROSTER_SET of
      { id : string
      ; target : Jid.t
      ; handle : string
      ; subscription : Rosters.subscription
      ; groups : string list }
  | SUBSCRIPTION_REQUEST of {id : string; ato : Jid.t}
  | PRESENCE_UPDATE of Rosters.availability
  | IQ_ERROR of
      { error_type : Actions.error_type
      ; error_tag : string
      ; ato : Jid.t
      ; id : string }

(** [to_string t] takes an event and returns it's string representation *)
val to_string : t -> string

(** [lift pr] converts the parse_result [pr] into an event type suitable for sending to the state machine *)
val lift : Parser.parse_result -> t
