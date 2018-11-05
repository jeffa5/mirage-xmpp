(** The type of a Stanza *)
type t

type tag =
  | Message
  | Presence
  | IQ

type name = string * string
type attribute = name * string

(** [create t] creates a new stanza with the given tag type *)
val create : tag -> t
(* functions to create a stanza? *)

(** [add_attr t a] updates the stanza [t] to include the attribute [a] *)
val add_attr : t -> attribute -> t

(** [add_content t t'] takes the initial stanza [t] and updates the content of it to include [t'] *)
val add_content : t -> t -> t

(** [to_string t] takes a stanza [t] and returns the string representation of it *)
val to_string : t -> string
