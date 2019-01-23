type name = string * string [@@deriving sexp]

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
  | Subscription of string
  | Mechanism of string
  | Other of string * string
[@@deriving sexp]

type attribute = string * attribute_value [@@deriving sexp]
type tag = name * attribute list [@@deriving sexp]

type t =
  | Text of string
  | Element of tag * t list
[@@deriving sexp]

val remove_prefixes : t -> t
val to_string : t -> string
val tag_to_string : empty:bool -> tag -> string
val create : ?children:t list -> tag -> t
