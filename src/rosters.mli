(** A roster stores information about contacts for a particular user, it also stores information about subscriptions to the user's presence. *)

module Subscription : sig
  type t =
    | None
    | To
    | From
    | Both
    | Remove

  val to_string : t -> string
end

module Presence : sig
  type t =
    | Online
    | Offline
  [@@deriving sexp]
end

module Item : sig
  type t =
    { handle : string
    ; subscription : Subscription.t [@default (None : Subscription.t)]
    ; ask : bool [@default false]
    ; groups : string list }
  [@@deriving sexp, make]

  val to_tuple : t -> string * Subscription.t * bool * string list
end

(** [lock_user user] attempts to acquire a basic lock on the bare user to prevent others from using it. It returns [true] if it is successful in acquiring the lock and [false] otherwise. *)
val lock_user : Jid.Bare.t -> bool Lwt.t

(** [unlock_user user] unlocks the lock set by [lock_user]. *)
val unlock_user : Jid.Bare.t -> unit Lwt.t

(** [remove_item user contact] removes the [contact] from the [user]'s roster. *)
val remove_item : Jid.Bare.t -> Jid.Bare.t -> unit Lwt.t

(** [downgrade_subscription_to user contact] removes the {e to} part of the presence subscription from [user] to [contact]. *)
val downgrade_subscription_to : Jid.Bare.t -> Jid.Bare.t -> unit Lwt.t

(** [downgrade_subscription_from user contact] removes the {e from} part of the presence subscription from [user] to [contact]. *)
val downgrade_subscription_from : Jid.Bare.t -> Jid.Bare.t -> unit Lwt.t

(** [upgrade_subscription_to user contact] adds the {e to} part of the presence subscription from [user] to [contact]. *)
val upgrade_subscription_to : Jid.Bare.t -> Jid.Bare.t -> unit Lwt.t

(** [upgrade_subscription_from user contact] adds the {e from} part of the presence subscription from [user] to [contact]. *)
val upgrade_subscription_from : Jid.Bare.t -> Jid.Bare.t -> unit Lwt.t

(** [unset_ask user contact] sets the [ask] value to false for the [contact] in the [user]'s roster. *)
val unset_ask : Jid.Bare.t -> Jid.Bare.t -> unit Lwt.t

(** [set_ask user contact] sets the [ask] value to true for the [contact] in the [user]'s roster. *)
val set_ask : Jid.Bare.t -> Jid.Bare.t -> unit Lwt.t

(** [set_item ~subscription ~handle ~groups user contact] sets the item for [contact] in the [user]'s roster either by creating a new item and inserting it or updating an existing item. The default for [subscription] is [None], for [handle] is [""] and for [groups] is [[]]. *)
val set_item :
     ?subscription:Subscription.t
  -> ?handle:string
  -> ?groups:string list
  -> Jid.Bare.t
  -> Jid.Bare.t
  -> Item.t Lwt.t

(** [get_presence user] gets the current presence status for the [user]. *)
val get_presence : Jid.Bare.t -> Presence.t Lwt.t

val set_presence : Jid.Bare.t -> Presence.t -> unit Lwt.t

(** [get_ask user contact] gets the current status of a presence subscription ask from [user] to [contact]. *)
val get_ask : Jid.Bare.t -> Jid.Bare.t -> bool option Lwt.t

(** [get_subscription user contact] gets the subscription from [user] to [contact] if there is one. *)
val get_subscription : Jid.Bare.t -> Jid.Bare.t -> Subscription.t option Lwt.t

(** [get_item user contact] get the item associated with the [contact] in the [user]'s roster if there is one. *)
val get_item : Jid.Bare.t -> Jid.Bare.t -> Item.t option Lwt.t

(** [get_items user] returns a list of [(contact, item)] pairs from the [user]'s roster. *)
val get_items : Jid.Bare.t -> (Jid.Bare.t * Item.t) list Lwt.t

(** [get_subscriptions user] gets the list of [contact]s where the [user] has a subscription to the [contact]. *)
val get_subscriptions : Jid.Bare.t -> Jid.Bare.t list Lwt.t

(** [get_subscribers user] gets the list of [contact]s where the [contact] has a subscription to the [user]. *)
val get_subscribers : Jid.Bare.t -> Jid.Bare.t list Lwt.t

(** [to_string ()] returns the string representation of the rosters. *)
val to_string : unit -> string Lwt.t

(** [clear ()] clears the roster. *)
val clear : unit -> unit Lwt.t
