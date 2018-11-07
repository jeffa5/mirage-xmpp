(** The type of a parser *)
type t

(** [create s] creates a new parser from the given stream of input characters *)
val create : char Lwt_stream.t -> t

(** [parse_stanza t] will act similarly to parse_xml apart from that it returns a full stanza or in the case of a start of stream, it returns a near-complete stanza *)
val parse_stanza : t -> (Stanza.t, string) result Lwt.t
