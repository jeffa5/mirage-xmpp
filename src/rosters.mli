(** A roster stores information about contacts for a particular user, it also stores information about subscriptions to the users' presence *)

type availability =
  | Online
  | Offline

type subscription =
  | None
  | From
  | Both

val availability_to_string : availability -> string
val subscription_to_string : subscription -> string
val subscription_of_string : string -> subscription
val set_presence : jid:Jid.t -> availability -> unit

val set_item :
     user_jid:Jid.t
  -> target_jid:Jid.t
  -> handle:string
  -> subscription:subscription
  -> groups:string list
  -> unit

val get : Jid.t -> (Jid.t * string * subscription * string list) list
val get_subscribers : Jid.t -> Jid.t list
val to_string : unit -> string
val clear : unit -> unit
