(** The module for actions generated by the state machine *)

type handler_actions =
  | RESET_PARSER
  | EXIT
[@@deriving sexp]

type error_type =
  | Auth
  | Cancel
  | Continue
  | Modify
  | Wait
[@@deriving sexp]

val error_type_to_string : error_type -> string

(** The type of actions, examples for now *)
type t =
  | SEND_STREAM_HEADER
  | SEND_STREAM_FEATURES_SASL
  | SEND_SASL_SUCCESS
  | SEND_STREAM_FEATURES
  | SERVER_GEN_RESOURCE_IDENTIFIER of string
  | SESSION_START_SUCCESS of string
  | CLOSE
  | ERROR of string
  | SET_JID of string
  | SET_JID_RESOURCE of {id : string; resource : string}
  | GET_ROSTER of string
  | SET_ROSTER of {id : string; target : Jid.t; handle : string; groups : string list}
  | PUSH_ROSTER of {ato : Jid.t option; contact : Jid.t}
  | ADD_TO_CONNECTIONS
  | REMOVE_FROM_CONNECTIONS
  | SUBSCRIPTION_REQUEST of {ato : Jid.t; xml : Xml.t; from : Jid.t option}
  | UPDATE_PRESENCE of {status : Rosters.presence; xml : Xml.t option}
  | SEND_PRESENCE_UPDATE of {from : Jid.t; xml : Xml.t option}
  | SEND_CURRENT_PRESENCE of Jid.t
  | IQ_ERROR of {error_type : error_type; error_tag : string; id : string}
  | MESSAGE of {ato : Jid.t; message : Xml.t}
  | ROSTER_REMOVE of {id : string; target : Jid.t}
  | SUBSCRIPTION_APPROVAL of {ato : Jid.t; xml : Xml.t; from : Jid.t option}
  | ROSTER_SET_FROM of Jid.t
  | PROBE_PRESENCE
  | SUBSCRIPTION_CANCELLATION of {user : Jid.t; force : bool}
  | SUBSCRIPTION_REMOVAL of {contact : Jid.t}
[@@deriving sexp]

(** [to_string t] takes an action and returns its string representation *)
val to_string : t -> string
