type t =
  | SEND_STREAM_HEADER of {from : Jid.t; ato : Jid.t}
  | SEND_STREAM_FEATURES
  | REPLY_STANZA of Stanza.t
  | SERVER_GEN_RESOURCE_IDENTIFIER of string
  | CLOSE
  | ERROR of string
  | SET_JID of Jid.t
  | SET_JID_RESOURCE of {id : string; resource : string}
  | GET_ROSTER of {from : Jid.t; id : string}
  | SET_ROSTER of
      { id : string
      ; from : Jid.t
      ; target : Jid.t
      ; handle : string
      ; subscription : string
      ; groups : string list }
  | PUSH_ROSTER of
      { jid : Jid.t
      ; target : Jid.t
      ; handle : string
      ; subscription : string
      ; groups : string list }
  | ADD_TO_CONNECTIONS
  | REMOVE_FROM_CONNECTIONS

let to_string = function
  | SEND_STREAM_HEADER {from; ato} ->
    "SEND_STREAM_HEADER: from=" ^ Jid.to_string from ^ " to=" ^ Jid.to_string ato
  | SEND_STREAM_FEATURES -> "SEND_STREAM_FEATURES"
  | REPLY_STANZA s -> "REPLY_STANZA: " ^ Utils.mask_id (Stanza.to_string s)
  | SERVER_GEN_RESOURCE_IDENTIFIER s -> "SERVER_GEN_RESOURCE_IDENTIFIER: " ^ s
  | CLOSE -> "CLOSE"
  | ERROR s -> "ERROR: " ^ s
  | SET_JID j -> "SET_JID: " ^ Jid.to_string j
  | SET_JID_RESOURCE {id; resource} ->
    "SET_JID_RESOURCE: id=" ^ id ^ " resource=" ^ resource
  | GET_ROSTER {id; from} -> "GET_ROSTER: id=" ^ id ^ " from=" ^ Jid.to_string from
  | SET_ROSTER {id; from; target; handle; subscription; groups} ->
    "SET_ROSTER: id="
    ^ id
    ^ " from="
    ^ Jid.to_string from
    ^ " target="
    ^ Jid.to_string target
    ^ " handle="
    ^ handle
    ^ " subscribed="
    ^ subscription
    ^ " groups=["
    ^ String.concat " " groups
    ^ "]"
  | PUSH_ROSTER {jid; target; handle; subscription; groups} ->
    "PUSH_ROSTER: jid="
    ^ Jid.to_string jid
    ^ " target="
    ^ Jid.to_string target
    ^ " handle="
    ^ handle
    ^ " subscription="
    ^ subscription
    ^ " groups=["
    ^ String.concat " " groups
    ^ "]"
  | ADD_TO_CONNECTIONS -> "ADD_TO_CONNECTIONS"
  | REMOVE_FROM_CONNECTIONS -> "REMOVE_FROM_CONNECTIONS"
;;
