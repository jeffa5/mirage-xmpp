(** The type of a parser *)
type t

(** [create s] creates a new parser from the given stream of input characters *)
val create : char Lwt_stream.t -> t

(** [parse_xml t] will repeatedly consume characters from the parser [t]. It will return a wrapped result of text to send back to the user or an error message *)
(* val parse_xml : t -> (string option, string) Result.result Lwt.t *)

(** [parse_stanza t] will act similarly to parse_xml apart from that it returns a full stanza or in the case of a start of stream, it returns a near-complete stanza *)
val parse_stanza : t -> Stanza.t
