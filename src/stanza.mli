(** The type of a Stanza. *)
type t =
  | Message of Xml.t
  | Presence of Xml.t
  | Iq of Xml.t

(** [to_xml t] returns the xml element contained within the stanza type [t]. *)
val to_xml : t -> Xml.t

(** [gen_id ()] generates a new string to use as an id. *)
val gen_id : unit -> string

(** [create_presence ~attributes ~atype ~ato ~id ~from children] creates a presence stanza with the given attributes and children. *)
val create_presence :
     ?attributes:Xml.attribute list
  -> ?atype:string
  -> ?ato:Jid.t
  -> id:string option
  -> from:Jid.t
  -> Xml.t list
  -> t

(** [create_iq ~attributes ~ato ~atype ~id children] creates an iq stanza with the given attributes and children. *)
val create_iq :
     ?attributes:Xml.attribute list
  -> ?ato:Jid.t
  -> atype:string
  -> id:string
  -> Xml.t list
  -> t

(** [create_iq_error ~from ~ato ~id ~error_type ~error_tag] creates an iq error stanza with the given attributes. *)
val create_iq_error :
     from:Jid.t
  -> ato:Jid.t
  -> id:string
  -> error_type:Actions.error_type
  -> error_tag:string
  -> t

(** [create_bind ~attributes children] creates an iq bind stanza with the given attributes and children. *)
val create_bind : ?attributes:Xml.attribute list -> Xml.t list -> Xml.t

(** [create_resource ~attributes children] creates a resource bind xml element with the given attributes and children. *)
val create_resource : ?attributes:Xml.attribute list -> Xml.t list -> Xml.t

val create_bind_result : id:string -> jid:Jid.t -> unit -> t

val create_roster_get_result :
  id:string -> ato:Jid.t -> (Jid.t * Rosters.Item.t) list -> t

val create_roster_set_result : id:string -> ato:Jid.t -> t
val create_roster_push : id:string -> ato:Jid.t -> Jid.t * Rosters.Item.t -> t

(** [to_string t] takes a stanza [t] and returns the string representation of it *)
val to_string : t -> string

val get_subscription : Xml.attribute list -> string option
val get_id_exn : Xml.attribute list -> string
val get_id : Xml.attribute list -> string option
val get_from : Xml.attribute list -> Jid.t
val get_to : Xml.attribute list -> Jid.t
val get_type : Xml.attribute list -> string option
val get_version : Xml.attribute list -> string
val get_jid : Xml.attribute list -> Jid.t
val get_name : Xml.attribute list -> string option
