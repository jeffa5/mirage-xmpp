(** The type of a Stanza *)
type t =
  | Message of Xml.t
  | Presence of Xml.t
  | Iq of Xml.t

val gen_id : unit -> string

val create_presence :
     ?attributes:Xml.attribute list
  -> ?atype:string
  -> ?id:string
  -> ato:Jid.t
  -> from:Jid.t
  -> Xml.t list
  -> t

val create_iq :
     ?attributes:Xml.attribute list
  -> ?ato:Jid.t
  -> atype:string
  -> id:string
  -> Xml.t list
  -> t

val create_bind : ?attributes:Xml.attribute list -> Xml.t list -> Xml.t
val create_resource : ?attributes:Xml.attribute list -> Xml.t list -> Xml.t
val create_bind_result : id:string -> jid:Jid.t -> unit -> t

val create_roster_get_result :
     id:string
  -> ato:Jid.t
  -> (Jid.t * string * Rosters.subscription * string list) list
  -> t

val create_roster_set_result : id:string -> ato:Jid.t -> t

val create_roster_push :
  id:string -> ato:Jid.t -> Jid.t * string * Rosters.subscription * string list -> t

(** [to_string t] takes a stanza [t] and returns the string representation of it *)
val to_string : t -> string

val get_id : Xml.attribute list -> string
val get_from : Xml.attribute list -> Jid.t
val get_to : Xml.attribute list -> Jid.t
val get_type : Xml.attribute list -> string option
val get_version : Xml.attribute list -> string
val get_jid : Xml.attribute list -> Jid.t
val get_name : Xml.attribute list -> string
