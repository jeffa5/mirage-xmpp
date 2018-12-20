(** A roster stores information about contacts for a particular user, it also stores information about subscriptions to the users' presence *)

val set_item :
     user_jid:Jid.t
  -> target_jid:Jid.t
  -> handle:string
  -> subscribed:bool
  -> groups:string list
  -> unit

val get_jids : Jid.t -> Jid.t list
val to_string : unit -> string
val clear : unit -> unit
