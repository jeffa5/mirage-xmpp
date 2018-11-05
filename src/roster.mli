(** A roster stores information about contacts for a particular user, it also stores information about subscriptions to the users' presence *)

(** The type of a roster *)
type t

(** Create a new global roster *)
val create : unit -> t

(** [get_user t j] returns a stanza built from the user's roster in [t] *)
val get_user : t -> Jid.t -> Stanza.t

val update : t -> Jid.t -> t
val delete : t -> Jid.t -> t
