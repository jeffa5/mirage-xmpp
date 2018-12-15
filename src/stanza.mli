(** The type of a Stanza *)
type t =
  | Message of Xml.t
  | Presence of Xml.t
  | Iq of Xml.t

val create_message : ?children:Xml.t list -> Xml.tag -> t
val create_presence : ?children:Xml.t list -> Xml.tag -> t
val create_iq : ?children:Xml.t list -> Xml.tag -> t
val create_iq_bind : ?children:Xml.t list -> string -> t
val create_iq_query : ?children:Xml.t list -> string -> string -> t

val create_stream_header :
     ?version:string
  -> ?lang:string
  -> ?xmlns:string
  -> ?stream_ns:string
  -> ?attributes:Xml.attribute list
  -> string
  -> string
  -> Xml.tag

(** [get_attribute_by_name_exn l a] searches [l] for an attribute with name equal to [a], throwing an exception Not_found if it is not available. *)
val get_attribute_by_name_exn : Xml.attribute list -> string -> Xml.attribute

(** [get_attribute_by_name l a] searches [l] for an attribute with name equal to [a] *)
val get_attribute_by_name : Xml.attribute list -> string -> Xml.attribute option

(** [to_string t] takes a stanza [t] and returns the string representation of it *)
val to_string : t -> string

val get_value : Xml.attribute -> string
val get_id : Xml.attribute list -> string
val get_from : Xml.attribute list -> string
