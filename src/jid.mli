(** The type of a Jabber ID *)
type t

(** [create s] creates a new jid from the string, splitting it appropriately *)
val create : string -> t

(** [full t] is a predicate on whether the JID is a full JID *)
val full : t -> bool
