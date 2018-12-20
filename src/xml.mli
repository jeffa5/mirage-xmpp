type name = string * string

type attribute_value =
  | From of Jid.t
  | To of Jid.t
  | Id of string
  | Jid of Jid.t
  | Xmlns of string
  | Type of string
  | Ver of string
  | Version of string
  | Lang of string
  | Stream of string
  | Name of string

type attribute = string * attribute_value
type tag = name * attribute list

type t =
  | Text of string
  | Element of tag * t list

val to_string : t -> string
val tag_to_string : empty:bool -> tag -> string
val create : ?children:t list -> tag -> t
