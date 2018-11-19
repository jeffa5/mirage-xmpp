(** The module to handle conversion of stanzas into events for the state machine *)

(** The type of events, examples for now *)
type t =
  | STANZA of Stanza.t
  | CLOSE

(** [lift s] converts the given stanza [s] into an event type *)
(* val lift : Stanza.t -> t *)

(** [to_string t] takes an event and returns it's string representation *)
val to_string : t -> string
