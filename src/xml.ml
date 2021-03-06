open Sexplib.Std

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

let remove_prefixes_attribute (_prefix, value) = "", value

let rec remove_prefixes = function
  | Element (((_prefix, name), attributes), children) ->
    Element
      ( (("", name), List.map remove_prefixes_attribute attributes)
      , List.map remove_prefixes children )
  | Text _t as text -> text
;;

let name_to_string (prefix, name) = if prefix <> "" then prefix ^ ":" ^ name else name

let attribute_to_string (namespace, nameval) =
  (if namespace <> "" then namespace ^ ":" else "")
  ^
  match nameval with
  | From jid -> "from='" ^ Jid.to_string jid ^ "'"
  | To jid -> "to='" ^ Jid.to_string jid ^ "'"
  | Id s -> "id='" ^ s ^ "'"
  | Jid jid -> "jid='" ^ Jid.to_string jid ^ "'"
  | Xmlns s -> "xmlns='" ^ s ^ "'"
  | Type s -> "type='" ^ s ^ "'"
  | Ver s -> "ver='" ^ s ^ "'"
  | Version s -> "version='" ^ s ^ "'"
  | Lang s -> "lang='" ^ s ^ "'"
  | Stream s -> "stream='" ^ s ^ "'"
  | Name s -> "name='" ^ s ^ "'"
  | Subscription s -> "subscription='" ^ s ^ "'"
  | Mechanism s -> "mechanism='" ^ s ^ "'"
  | Other (name, value) -> name ^ "='" ^ value ^ "'"
;;

let tag_to_string ~empty (name, attributes) =
  let sep = " " in
  let attr_string =
    String.concat sep (List.map (fun a -> attribute_to_string a) attributes)
  in
  let name_string = name_to_string name in
  "<"
  ^ name_string
  ^ (if attr_string <> "" then sep ^ attr_string else "")
  ^ if empty then "/>" else ">"
;;

let rec to_string = function
  | Text s -> s
  | Element (((name, _attributes) as tag), children) ->
    (match children with
    | [] -> tag_to_string ~empty:true tag
    | cs ->
      tag_to_string ~empty:false tag
      ^ String.concat "" (List.map (fun c -> to_string c) cs)
      ^ "</"
      ^ name_to_string name
      ^ ">")
;;

let create ?(children = []) tag = Element (tag, children)
