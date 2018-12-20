(* This will contain the parser type, roster type and connections type *)

(** The type of an XMPP handler. *)
type t
(* The idea is that this will be called when the on_connect triggers in the unikernel so this will then handle *)

(* When called this has a few jobs:
   - create the parser with the given stream
   - initialise the new handler type with the parameters
*)

(** [create c r s f] creates a new handler. [c] is a connections table of the currently active connections to the server. [r] is the roster for the server. [s] is a stream to receive the incoming data on. [f] is a callback function which can be used to send data back to the user *)
val create :
     connections:Connections.t ref
  -> stream:char Lwt_stream.t
  -> callback:(string option -> unit)
  -> t

(** [handle t] takes the handler and starts handling the XMPP connection with the client.

    This controls the main operation of the server:
    - call parse_stanza on the parser which will return a new stanza
    - translates the received stanza into an event type and pass this to the fsm to get a new fsm
    - take the actions from the fsm and push necessary data to the callback function, handle roster events and perform lookups on the connections in order to send data to other users
    - call parse_stanza again and repeat
*)
val handle : t -> unit Lwt.t

val to_string : t -> string
