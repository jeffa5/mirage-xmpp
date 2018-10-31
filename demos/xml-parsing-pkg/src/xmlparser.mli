(** A module to handle parsing of given xml streams. Generates error messages to enforce strict xml and does not do error recovery. *)

(** [parse_xml s] will make an xml parser from the given stream and repeatedly consume characters from it. It will return a wrapped result of text to send back to the user or an error message *)
val parse_xml : char Lwt_stream.t -> (string option, string) Result.result Lwt.t
