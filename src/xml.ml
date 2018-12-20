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
