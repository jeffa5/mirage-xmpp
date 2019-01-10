(** State machine representing the transitions for the XMPP input events. The events drive the new states of the state machine and it returns actions to be taken, typically of the form of writing data back to the user. *)

(** The type of a state machine *)
type t

(** Create a state machine in the initial state *)
val initial : t

(** [handle t e] updates the state machine [t] with the event [e] to give the resulting state machine in a new state and the list of actions to be performed *)
val handle : t -> Events.t -> t * Actions.t list * Actions.handler_actions list

val to_string : t -> string
