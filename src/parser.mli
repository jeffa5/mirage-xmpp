(** The type of a parser *)
type t

type parse_result =
  | Stanza of Stanza.t
  | Sasl_auth of Xml.t
  | Stream_Element of Stream.t
  | Error of string

(** [create s] creates a new parser from the given stream of input characters *)
val create : char Lwt_stream.t -> t

val reset : t -> t

(** [parse_stanza t] will act similarly to parse_xml apart from that it returns a full stanza or in the case of a start of stream, it returns a near-complete stanza *)
val parse : t -> parse_result Lwt.t
