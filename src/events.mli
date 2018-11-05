(** The module to handle conversion of stanzas into events for the state machine *)

(** The type of events, examples for now *)
type t =
  | STARTING
  | CLOSING

(** [lift s] converts the given stanza [s] into an event type *)
val lift : Stanza.t -> t
