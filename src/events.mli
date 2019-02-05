(** The module to handle conversion of stanzas into events for the state machine *)

(** The type of events, examples for now *)
type t =
  | STREAM_HEADER of {version : string}
  | SASL_AUTH of {user : string; password : string}
  | ANONYMOUS_SASL_AUTH
  | RESOURCE_BIND_SERVER_GEN of {id : string}
  | RESOURCE_BIND_CLIENT_GEN of {id : string; resource : string}
  | SESSION_START of string
  | STREAM_CLOSE
  | ERROR of string
  | ROSTER_GET of string
  | ROSTER_SET of {id : string; target : Jid.t; handle : string; groups : string list}
  | ROSTER_REMOVE of {id : string; target : Jid.t}
  | PRESENCE_UPDATE of {status : Rosters.Presence.t; xml : Xml.t option}
  | IQ_ERROR of {error_type : Actions.error_type; error_tag : string; id : string}
  | MESSAGE of {ato : Jid.t; message : Xml.t}
  | LOG_OUT
  | NOOP
  | SUBSCRIPTION_REQUEST of {ato : Jid.t; xml : Xml.t}
  | SUBSCRIPTION_APPROVAL of {ato : Jid.t; xml : Xml.t}
  | SUBSCRIPTION_CANCELLATION of {user : Jid.t}
  | SUBSCRIPTION_REMOVAL of {contact : Jid.t}
[@@deriving sexp]

(** [to_string t] takes an event and returns it's string representation *)
val to_string : t -> string

(** [lift pr] converts the parse_result [pr] into an event type suitable for sending to the state machine *)
val lift : Parser.parse_result -> t
