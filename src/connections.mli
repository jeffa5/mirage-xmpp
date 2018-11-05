(** Need to store active connections in order to be able to send data to them from other users *)

(** The type of connections *)
type t

(** Create a connections table *)
val create : unit -> t

(** [add t j f] adds [j] and [f] to [t] and returns a new [t] with them added. [f] is the push function to the stream for that user *)
val add : t -> Jid.t -> (string -> unit) -> t

(** [get t j] returns the push function associated with the [j] in the connections list if it is present *)
val get : t -> Jid.t -> (string -> unit) option

(** [remove t j] removes the jid [j] entry from the table if present *)
val remove : t -> Jid.t -> t
