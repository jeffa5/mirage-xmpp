(** The type of a Jabber ID *)
type bare_jid = string * string

type full_jid = bare_jid * string

type t =
  | Full_JID of full_jid
  | Bare_JID of bare_jid
  | Domain of string
[@@deriving sexp]

val at_least_bare : t -> bool
val to_bare : t -> t

(** [of_string s] creates a new jid from the string, splitting it appropriately *)
val of_string : string -> t

(** [to_string t] returns the string representation of t *)
val to_string : t -> string

val compare : t -> t -> int
val create_resource : unit -> string
val set_resource : string -> t -> t
