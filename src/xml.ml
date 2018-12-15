type name = string * string
type attribute = name * string
type tag = name * attribute list

type t =
  | Text of string
  | Element of tag * t list

let name_to_string (prefix, name) = if prefix <> "" then prefix ^ ":" ^ name else name
let attribute_to_string (name, attribute) = name_to_string name ^ "='" ^ attribute ^ "'"

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
