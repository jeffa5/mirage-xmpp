type name = string * string
type attribute = name * string
type tag = name * attribute list

type t =
  | Text of string
  | Element of tag * t list

val to_string : t -> string
val tag_to_string : empty:bool -> tag -> string
val create : ?children:t list -> tag -> t
