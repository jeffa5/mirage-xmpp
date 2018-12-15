open Astring

type t =
  | Message of Xml.t
  | Presence of Xml.t
  | Iq of Xml.t

let create_message ?(children = []) tag = Message (Element (tag, children))
let create_presence ?(children = []) tag = Presence (Element (tag, children))
let create_iq ?(children = []) tag = Iq (Element (tag, children))

let create_iq_bind ?(children = []) id =
  create_iq
    (("", "iq"), [("", "id"), id; ("", "type"), "result"])
    ~children:
      [ Xml.create
          (("", "bind"), [("", "xmlns"), "urn:ietf:params:xml:ns:xmpp-bind"])
          ~children ]
;;

let create_iq_query ?(children = []) id from =
  create_iq
    (("", "iq"), [("", "id"), id; ("", "to"), from; ("", "type"), "result"])
    ~children:
      [ Xml.create
          (("", "query"), [("", "xmlns"), "jabber:iq:roster"; ("", "ver"), "ver7"])
          ~children ]
;;

let get_attribute_by_name_exn attributes a =
  let rec aux = function
    | [] -> raise Not_found
    | (((_, name), _) as attr) :: attrs -> if name = a then attr else aux attrs
  in
  aux attributes
;;

let get_attribute_by_name attributes a =
  let rec aux = function
    | [] -> None
    | (((_, name), _) as attr) :: attrs -> if name = a then Some attr else aux attrs
  in
  aux attributes
;;

let get_value (_, value) = value
let get_id attrs = get_value (get_attribute_by_name_exn attrs "id")
let get_from attrs = get_value (get_attribute_by_name_exn attrs "from")
let name_to_string (prefix, name) = if prefix <> "" then prefix ^ ":" ^ name else name

let pp_attribute_to_string (name, attribute) =
  let name_string = name_to_string name in
  if name_string = "id"
  then name_string ^ "='redacted_for_testing'"
  else name_string ^ "='" ^ attribute ^ "'"
;;

let pp_tag_to_string (name, attributes) =
  let sep = "\n  " in
  let attr_string =
    String.concat ~sep (List.map (fun a -> pp_attribute_to_string a) attributes)
  in
  let name_string = name_to_string name in
  name_string ^ if attr_string <> "" then sep ^ attr_string else ""
;;

let to_string = function
  | Message xml -> Xml.to_string xml
  | Presence xml -> Xml.to_string xml
  | Iq xml -> Xml.to_string xml
;;

let gen_id () = Uuidm.(to_string (create `V4))

let create_stream_header
    ?(version = "1.0")
    ?(lang = "en")
    ?(xmlns = "jabber:client")
    ?(stream_ns = "http://etherx.jabber.org/streams")
    ?(attributes = [])
    from
    dest =
  ( ("stream", "stream")
  , [ ("", "from"), from
    ; ("", "id"), gen_id ()
    ; ("", "to"), dest
    ; ("", "version"), version
    ; ("xml", "lang"), lang
    ; ("", "xmlns"), xmlns
    ; ("xmlns", "stream"), stream_ns ]
    @ attributes )
;;

let%expect_test "empty tag prefix" =
  let tag = ("", "name"), [] in
  print_endline (pp_tag_to_string tag);
  [%expect {| name |}]
;;

let%expect_test "create tag" =
  let tag = ("prefix", "name"), [] in
  print_endline (pp_tag_to_string tag);
  [%expect {| prefix:name |}]
;;

let%expect_test "empty tag prefix with attrs" =
  let tag = ("", "name"), [("", "attr1"), "val1"] in
  print_endline (pp_tag_to_string tag);
  [%expect {|
    name
      attr1='val1' |}]
;;

let%expect_test "create tag with attrs" =
  let tag = ("prefix", "name"), [("prefix", "attr1"), "val1"] in
  print_endline (pp_tag_to_string tag);
  [%expect {|
    prefix:name
      prefix:attr1='val1' |}]
;;
