type t =
  | Header of Xml.tag
  | Features
  | Error
  | Close
[@@deriving sexp]

val to_string : t -> string
val features_sasl_mechanisms : Xml.t
val features : Xml.t

val create_header :
     ?version:string
  -> ?lang:string
  -> ?xmlns:string
  -> ?stream_ns:string
  -> ?attributes:Xml.attribute list
  -> ?ato:Jid.t
  -> ?from:Jid.t
  -> unit
  -> Xml.tag
