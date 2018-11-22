(** A roster stores information about contacts for a particular user, it also stores information about subscriptions to the users' presence *)

(** The type of a contact in the roster *)
type contact =
  { nickname : string
  ; group : string
  ; presence : bool }

(** The type of a roster *)
type t

(** An empty roster *)
val empty : t

val set_presence : t -> Jid.t -> bool -> t
val get_presence : t -> Jid.t -> bool
val set_contact : t -> Jid.t -> contact -> t
val get_contact : t -> Jid.t -> contact
val to_string : t -> string
val get_jids : t -> Jid.t list
