(** The type of a Jabber ID *)
type t

(** [full t] is a predicate on whether the JID is a full JID *)
val full : t -> bool

(** [of_string s] creates a new jid from the string, splitting it appropriately *)
val of_string : string -> t

(** [to_string t] returns the string representation of t *)
val to_string : t -> string
