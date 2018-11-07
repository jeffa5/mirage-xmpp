(** The type of a Stanza *)
type t

type name = string * string
type attribute = name * string
type tag = name * attribute list

(** [create ~children t] creates a new stanza with the given tag type and any children of the tag, defaults to empty list *)
val create : ?children:t list -> tag -> t

(** Make a stanza containing only text *)
val text : string list -> t

(** [add_attr t a] updates the stanza [t] to include the attribute [a] *)
val add_attr : tag -> attribute -> tag

val add_attrs : tag -> attribute list -> tag

(** [add_content t t'] takes the initial stanza [t] and updates the content of it to include [t'] *)
val add_content : t -> t -> t

(** [to_string t] takes a stanza [t] and returns the string representation of it *)
val to_string : t -> string
