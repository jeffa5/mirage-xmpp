(** The type of a Jabber ID *)

type t

val empty : t

(** [of_string s] creates a new jid from the string, splitting it appropriately *)
val of_string : string -> t

(** [to_string t] returns the string representation of t *)
val to_string : t -> string

val compare : t -> t -> int
val create_resource : unit -> string
val set_resource : string -> t -> t
