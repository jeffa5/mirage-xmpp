(** A roster stores information about contacts for a particular user, it also stores information about subscriptions to the user's presence. *)

(** Type for a users presence status. *)
type presence =
  | Online
  | Offline
[@@deriving sexp]

(** Type of a [user]'s subscription to the [contact]. *)
type subscription =
  | None  (** No subscription between the [user] and the [contact] *)
  | To  (** A subscription from the [user] to the [contact] *)
  | From  (** A subscription to the [user] from the [contact] *)
  | Both  (** A mutual subscription between the [user] and the [contact] *)
  | Remove  (** An attribute value in presence stanzas, not for use in the roster *)
[@@deriving sexp]

(** Type of an item in the [user]'s roster. *)
type item =
  { handle : string  (** The nickname given to the [contact] *)
  ; subscription : subscription
        (** The subscription status of the [user] to the [contact] *)
  ; ask : bool  (** Whether a subscription is currently being asked for *)
  ; groups : string list  (** The list of groups the [contact] is in *) }

(** [item_to_tuple i] converts the record [i] into a tuple of the parts in the order [(handle, subscription, ask, groups)]. *)
val item_to_tuple : item -> string * subscription * bool * string list

(** [presence_to_string p] returns the string representation of [p]. *)
val presence_to_string : presence -> string

(** [subscription_to_string s] returns the string representation of [s]. *)
val subscription_to_string : subscription -> string

(** [set_presence user p] sets the presence of the user [user] to presence [p]. *)
val set_presence : Jid.t -> presence -> unit

(** [remove_item user contact] removes the [contact] from the [user]'s roster. *)
val remove_item : Jid.t -> Jid.t -> unit

(** [downgrade_subscription_to user contact] removes the {e to} part of the presence subscription from [user] to [contact]. *)
val downgrade_subscription_to : Jid.t -> Jid.t -> unit

(** [downgrade_subscription_from user contact] removes the {e from} part of the presence subscription from [user] to [contact]. *)
val downgrade_subscription_from : Jid.t -> Jid.t -> unit

(** [upgrade_subscription_to user contact] adds the {e to} part of the presence subscription from [user] to [contact]. *)
val upgrade_subscription_to : Jid.t -> Jid.t -> unit

(** [upgrade_subscription_from user contact] adds the {e from} part of the presence subscription from [user] to [contact]. *)
val upgrade_subscription_from : Jid.t -> Jid.t -> unit

(** [unset_ask user contact] sets the [ask] value to false for the [contact] in the [user]'s roster. *)
val unset_ask : Jid.t -> Jid.t -> unit

(** [set_ask user contact] sets the [ask] value to true for the [contact] in the [user]'s roster. *)
val set_ask : Jid.t -> Jid.t -> unit

(** [set_item ~subscription ~handle ~groups user contact] sets the item for [contact] in the [user]'s roster either by creating a new item and inserting it or updating an existing item. The default for [subscription] is [None], for [handle] is [""] and for [groups] is [[]]. *)
val set_item :
     ?subscription:subscription
  -> ?handle:string
  -> ?groups:string list
  -> Jid.t
  -> Jid.t
  -> unit

(** [get_presence user] gets the current presence status for the [user]. *)
val get_presence : Jid.t -> presence

(** [get_ask user contact] gets the current status of a presence subscription ask from [user] to [contact]. *)
val get_ask : Jid.t -> Jid.t -> bool option

(** [get_subscription user contact] gets the subscription from [user] to [contact] if there is one. *)
val get_subscription : Jid.t -> Jid.t -> subscription option

(** [get_roster_item user contact] get the item associated with the [contact] in the [user]'s roster if there is one. *)
val get_roster_item : Jid.t -> Jid.t -> item option

(** [get_roster_items user] returns a list of [(contact, item)] pairs from the [user]'s roster. *)
val get_roster_items : Jid.t -> (Jid.t * item) list

(** [get_subscriptions user] gets the list of [contact]s where the [user] has a subscription to the [contact]. *)
val get_subscriptions : Jid.t -> Jid.t list

(** [get_subscribers user] gets the list of [contact]s where the [contact] has a subscription to the [user]. *)
val get_subscribers : Jid.t -> Jid.t list

(** [to_string ()] returns the string representation of the rosters. *)
val to_string : unit -> string

(** [clear ()] clears the roster. *)
val clear : unit -> unit
