type name = string * string
type attribute = name * string
type tag = name * attribute list

(** The type of a Stanza *)
type t =
  | Stanza of tag * t list
  | Text of string list

(** [create ~children t] creates a new stanza with the given tag type and any children of the tag, defaults to empty list *)
val create : ?children:t list -> tag -> t

val create_iq_bind : ?children:t list -> string -> t

(** Make a stanza containing only text *)
val text : string list -> t

val attribute : ?prefix:string -> string -> string -> attribute

val stream_header :
     ?version:string
  -> ?lang:string
  -> ?xmlns:string
  -> ?stream_ns:string
  -> ?attributes:attribute list
  -> string
  -> string
  -> t

val features : t

(** [add_attr t a] updates the stanza [t] to include the attribute [a] *)
val add_attr : tag -> attribute -> tag

val add_attrs : tag -> attribute list -> tag

(** [add_content t t'] takes the initial stanza [t] and updates the content of it to include [t'] *)
val add_content : t -> t -> t

(** [get_attribute_by_name_exn l a] searches [l] for an attribute with name equal to [a], throwing an exception Not_found if it is not available. *)
val get_attribute_by_name_exn : attribute list -> string -> attribute

(** [get_attribute_by_name l a] searches [l] for an attribute with name equal to [a] *)
val get_attribute_by_name : attribute list -> string -> attribute option

(** [to_string t] takes a stanza [t] and returns the string representation of it *)
val to_string : ?auto_close:bool -> t -> string

(** Similar to [to_string] but formats the string prettily *)
val pp_to_string : ?auto_close:bool -> t -> string

val get_value : attribute -> string
val get_id : attribute list -> string
