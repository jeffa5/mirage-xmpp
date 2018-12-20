type t =
  | Header of Xml.tag
  | Features
  | Error
  | Close

val to_string : t -> string
val features : Xml.t

val create_header :
     ?version:string
  -> ?lang:string
  -> ?xmlns:string
  -> ?stream_ns:string
  -> ?attributes:Xml.attribute list
  -> Jid.t
  -> Jid.t
  -> Xml.tag
