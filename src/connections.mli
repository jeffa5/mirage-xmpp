(** Need to store active connections in order to be able to send data to them from other users *)

(** The type of connections *)
type t

(** Create an empty connections table *)
val empty : t

(** [add t j f] adds [j] and [f] to [t] and returns a new [t] with them added. [f] is the push function to the stream for that user *)
val add : Jid.t -> (Actions.t option -> unit) -> t -> t

(** [get t j] returns the push function associated with the [j] in the connections list if it is present *)
val find : Jid.t -> t -> (Actions.t option -> unit) option

(** [remove t j] removes the jid [j] entry from the table if present *)
val remove : Jid.t -> t -> t

val to_string : t -> string
