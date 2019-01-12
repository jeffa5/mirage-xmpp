(** A roster stores information about contacts for a particular user, it also stores information about subscriptions to the users' presence *)

type availability =
  | Online
  | Offline

type subscription =
  | None
  | To
  | From
  | Both
  | Remove

type item =
  { handle : string
  ; subscription : subscription
  ; ask : bool
  ; groups : string list }

val item_to_tuple : item -> string * subscription * bool * string list
val availability_to_string : availability -> string
val subscription_to_string : subscription -> string
val subscription_of_string : string -> subscription
val set_presence : jid:Jid.t -> availability -> unit
val remove_item : Jid.t -> Jid.t -> unit
val downgrade_subscription_to : Jid.t -> Jid.t -> unit
val downgrade_subscription_from : Jid.t -> Jid.t -> unit
val upgrade_subscription_to : Jid.t -> Jid.t -> unit
val upgrade_subscription_from : Jid.t -> Jid.t -> unit
val unset_ask : Jid.t -> Jid.t -> unit
val set_ask : Jid.t -> Jid.t -> unit
val set_item : user:Jid.t -> contact:Jid.t -> handle:string -> groups:string list -> unit
val get_presence : Jid.t -> availability
val get_ask : Jid.t -> Jid.t -> bool
val get_subscription : Jid.t -> Jid.t -> subscription option
val get_roster_item : Jid.t -> Jid.t -> item option
val get_roster_items : Jid.t -> (Jid.t * item) list
val get_subscriptions : Jid.t -> Jid.t list
val get_subscribers : Jid.t -> Jid.t list
val to_string : unit -> string
val clear : unit -> unit
