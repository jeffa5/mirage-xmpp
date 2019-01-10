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
  | SET_ROSTER of {id : string; target : Jid.t; handle : string; groups : string list}
  | PUSH_ROSTER of {ato : Jid.t option; contact : Jid.t}
  | ADD_TO_CONNECTIONS
  | REMOVE_FROM_CONNECTIONS
  | SUBSCRIPTION_REQUEST of {ato : Jid.t; xml : Xml.t; from : Jid.t option}
  | UPDATE_PRESENCE of Rosters.availability
  | SEND_PRESENCE_UPDATE of Jid.t
  | SEND_CURRENT_PRESENCE of Jid.t
  | IQ_ERROR of {error_type : error_type; error_tag : string; id : string}
  | MESSAGE of {ato : Jid.t; message : Xml.t}
  | ROSTER_REMOVE of {id : string; target : Jid.t}
  | SUBSCRIPTION_APPROVAL of {ato : Jid.t; xml : Xml.t; from : Jid.t option}
  | ROSTER_SET_FROM of Jid.t

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
  | SET_ROSTER {id; target; handle; groups} ->
    "SET_ROSTER: id="
    ^ id
    ^ " target="
    ^ Jid.to_string target
    ^ " handle="
    ^ handle
    ^ " groups=["
    ^ String.concat " " groups
    ^ "]"
  | PUSH_ROSTER {ato; contact} ->
    "PUSH_ROSTER: to="
    ^ (match ato with Some j -> "Some " ^ Jid.to_string j | None -> "None")
    ^ " contact="
    ^ Jid.to_string contact
    ^ "]"
  | ADD_TO_CONNECTIONS -> "ADD_TO_CONNECTIONS"
  | REMOVE_FROM_CONNECTIONS -> "REMOVE_FROM_CONNECTIONS"
  | SUBSCRIPTION_REQUEST {ato; xml; from} ->
    "SUBSCRIPTION_REQUEST: to="
    ^ Jid.to_string ato
    ^ " xml="
    ^ Xml.to_string xml
    ^ " from="
    ^ (match from with Some f -> Jid.to_string f | None -> "")
  | UPDATE_PRESENCE availability ->
    "UPDATE_PRESENCE: availability=" ^ Rosters.availability_to_string availability
  | SEND_PRESENCE_UPDATE from -> "SEND_PRESENCE_UPDATE: from=" ^ Jid.to_string from
  | SEND_CURRENT_PRESENCE ato -> "SEND_CURRENT_PRESENCE: to=" ^ Jid.to_string ato
  | IQ_ERROR {error_type; error_tag; id} ->
    "IQ_ERROR: error_type="
    ^ error_type_to_string error_type
    ^ " error_tag="
    ^ error_tag
    ^ " id="
    ^ id
  | MESSAGE {ato; message} ->
    "MESSAGE: to=" ^ Jid.to_string ato ^ " message=" ^ Xml.to_string message
  | ROSTER_REMOVE {id; target} ->
    "ROSTER_REMOVE id=" ^ id ^ " target=" ^ Jid.to_string target
  | SUBSCRIPTION_APPROVAL {ato; xml; from} ->
    "SUBSCRIPTION_APPROVAL: to="
    ^ Jid.to_string ato
    ^ " xml="
    ^ Xml.to_string xml
    ^ " from="
    ^ (match from with Some f -> Jid.to_string f | None -> "")
  | ROSTER_SET_FROM from -> "ROSTER_SET_FROM from=" ^ Jid.to_string from
;;
