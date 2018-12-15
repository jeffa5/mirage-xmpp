type t =
  | Header of Xml.tag
  | Features
  | Error
  | Close

val to_string : t -> string
val features : Xml.t
