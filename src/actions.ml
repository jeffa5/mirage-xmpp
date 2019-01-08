type handler_actions = RESET_PARSER

let handler_actions_to_string = function RESET_PARSER -> "RESET_PARSER"

type error_type =
  | Auth
  | Cancel
  | Continue
  | Modify
  | Wait

let error_type_to_string = function
  | Auth -> "auth"
  | Cancel -> "cancel"
  | Continue -> "continue"
  | Modify -> "modify"
  | Wait -> "wait"
;;

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
  | SET_ROSTER of
      { id : string
      ; target : Jid.t
      ; handle : string
      ; subscription : Rosters.subscription
      ; groups : string list }
  | PUSH_ROSTER of
      { jid : Jid.t option
      ; target : Jid.t
      ; handle : string
      ; subscription : Rosters.subscription
      ; groups : string list }
  | ADD_TO_CONNECTIONS
  | REMOVE_FROM_CONNECTIONS
  | SUBSCRIPTION_REQUEST of {id : string; ato : Jid.t}
  | UPDATE_PRESENCE of Rosters.availability
  | SEND_PRESENCE_UPDATE of Jid.t
  | IQ_ERROR of {error_type : error_type; error_tag : string; id : string}

let to_string = function
  | SEND_STREAM_HEADER -> "SEND_STREAM_HEADER"
  | SEND_STREAM_FEATURES_SASL -> "SEND_STREAM_FEATURES_SASL"
  | SEND_SASL_SUCCESS -> "SEND_SASL_SUCCESS"
  | SEND_STREAM_FEATURES -> "SEND_STREAM_FEATURES"
  | SERVER_GEN_RESOURCE_IDENTIFIER s -> "SERVER_GEN_RESOURCE_IDENTIFIER: " ^ s
  | SESSION_START_SUCCESS id -> "SESSION_START_SUCCESS: id=" ^ id
  | CLOSE -> "CLOSE"
  | ERROR s -> "ERROR: " ^ s
  | SET_JID j -> "SET_JID: " ^ j
  | SET_JID_RESOURCE {id; resource} ->
    "SET_JID_RESOURCE: id=" ^ id ^ " resource=" ^ resource
  | GET_ROSTER id -> "GET_ROSTER: id=" ^ id
  | SET_ROSTER {id; target; handle; subscription; groups} ->
    "SET_ROSTER: id="
    ^ id
    ^ " target="
    ^ Jid.to_string target
    ^ " handle="
    ^ handle
    ^ " subscribed="
    ^ Rosters.subscription_to_string subscription
    ^ " groups=["
    ^ String.concat " " groups
    ^ "]"
  | PUSH_ROSTER {jid; target; handle; subscription; groups} ->
    "PUSH_ROSTER: jid="
    ^ (match jid with Some j -> "Some " ^ Jid.to_string j | None -> "None")
    ^ " target="
    ^ Jid.to_string target
    ^ " handle="
    ^ handle
    ^ " subscription="
    ^ Rosters.subscription_to_string subscription
    ^ " groups=["
    ^ String.concat " " groups
    ^ "]"
  | ADD_TO_CONNECTIONS -> "ADD_TO_CONNECTIONS"
  | REMOVE_FROM_CONNECTIONS -> "REMOVE_FROM_CONNECTIONS"
  | SUBSCRIPTION_REQUEST {id; ato} ->
    "SUBSCRIPTION_REQUEST: id=" ^ id ^ " to=" ^ Jid.to_string ato
  | UPDATE_PRESENCE availability ->
    "UPDATE_PRESENCE: availability=" ^ Rosters.availability_to_string availability
  | SEND_PRESENCE_UPDATE from -> "SEND_PRESENCE_UPDATE: from=" ^ Jid.to_string from
  | IQ_ERROR {error_type; error_tag; id} ->
    "IQ_ERROR: error_type="
    ^ error_type_to_string error_type
    ^ " error_tag="
    ^ error_tag
    ^ " id="
    ^ id
;;
