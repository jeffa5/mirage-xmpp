(** A roster stores information about contacts for a particular user, it also stores information about subscriptions to the users' presence *)

(** The type of a roster *)
type t

(** An empty roster *)
val empty : t

val update : t -> Jid.t -> bool -> t
val delete : t -> Jid.t -> t
val to_string : t -> string
