open Astring

type name = string * string
type attribute = name * string
type tag = name * attribute list

type t =
  | Stanza of tag * t list
  | Text of string list

let create ?(children = []) tag = Stanza (tag, children)

let create_iq_bind ?(children = []) id =
  create
    (("", "iq"), [("", "id"), id; ("", "type"), "result"])
    ~children:
      [ create
          (("", "bind"), [("", "xmlns"), "urn:ietf:params:xml:ns:xmpp-bind"])
          ~children ]
;;

let create_iq_query ?(children = []) id from =
  create
    (("", "iq"), [("", "id"), id; ("", "to"), from; ("", "type"), "result"])
    ~children:
      [ create
          (("", "query"), [("", "xmlns"), "jabber:iq:roster"; ("", "ver"), "ver7"])
          ~children ]
;;

let text s = Text s
let attribute ?(prefix = "") name value = (prefix, name), value

let add_attrs (name, l) attrs =
  let new_attrs = l @ attrs in
  name, new_attrs
;;

let add_attr tag attr = add_attrs tag [attr]

let add_content stanza t =
  match stanza with
  | Stanza (tag, l) ->
    let new_content = l @ [t] in
    Stanza (tag, new_content)
  | Text _ -> assert false
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

let pp_to_string ?(auto_close = true) s =
  let rec aux s depth =
    let indent =
      String.v ~len:(depth * 2) (fun _ ->
          match String.to_char " " with Some c -> c | None -> assert false )
    in
    match s with
    | Stanza ((name, attrs), l) ->
      let tag_string = pp_tag_to_string (name, attrs) in
      (match l with
      | [] ->
        if auto_close
        then indent ^ "<" ^ tag_string ^ "/>"
        else indent ^ "<" ^ tag_string ^ ">"
      | ss ->
        let stanzas =
          String.concat ~sep:"\n" (List.map (fun s -> aux s (depth + 1)) ss)
        in
        let start_tag = indent ^ "<" ^ tag_string ^ ">" in
        let end_tag = indent ^ "</" ^ name_to_string name ^ ">" in
        start_tag ^ "\n" ^ stanzas ^ "\n" ^ end_tag)
    | Text ss -> String.concat ~sep:"\n" (List.map (fun s -> indent ^ s) ss)
  in
  aux s 0
;;

let attribute_to_string (name, attribute) =
  let name_string = name_to_string name in
  name_string ^ "='" ^ attribute ^ "'"
;;

let tag_to_string (name, attributes) =
  let attr_string =
    String.concat ~sep:" " (List.map (fun a -> attribute_to_string a) attributes)
  in
  let name_string = name_to_string name in
  name_string ^ if attr_string <> "" then " " ^ attr_string else ""
;;

let to_string ?(auto_close = true) s =
  let rec aux = function
    | Stanza ((name, attrs), l) ->
      let tag_string = tag_to_string (name, attrs) in
      (match l with
      | [] -> if auto_close then "<" ^ tag_string ^ "/>" else "<" ^ tag_string ^ ">"
      | ss ->
        let stanzas = String.concat ~sep:"" (List.map (fun s -> aux s) ss) in
        let start_tag = "<" ^ tag_string ^ ">" in
        let end_tag = "</" ^ name_to_string name ^ ">" in
        start_tag ^ stanzas ^ end_tag)
    | Text ss -> String.concat ~sep:"\n" ss
  in
  aux s
;;

let gen_id () = Uuidm.(to_string (create `V4))

let stream_header
    ?(version = "1.0")
    ?(lang = "en")
    ?(xmlns = "jabber:client")
    ?(stream_ns = "http://etherx.jabber.org/streams")
    ?(attributes = [])
    from
    dest =
  create
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

let features =
  create
    (("stream", "features"), [])
    ~children:[create (("", "bind"), [("", "xmlns"), "urn:ietf:params:xml:ns:xmpp-bind"])]
;;

let%expect_test "empty tag prefix" =
  let tag = ("", "name"), [] in
  print_endline (pp_tag_to_string tag);
  [%expect {| name |}]
;;

let%expect_test "empty prefix in stanza" =
  let stanza = Stanza ((("", "name"), []), []) in
  print_endline (pp_to_string stanza);
  [%expect {| <name/> |}]
;;

let%expect_test "create" =
  let stanza = create (("prefix", "name"), []) in
  print_endline (pp_to_string stanza);
  [%expect {| <prefix:name/> |}]
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

let%expect_test "empty prefix in stanza with attrs" =
  let stanza = Stanza ((("", "name"), [("", "attr1"), "val1"]), []) in
  print_endline (pp_to_string stanza);
  [%expect {|
    <name
      attr1='val1'/> |}]
;;

let%expect_test "create stanza with attrs" =
  let stanza = create (("prefix", "name"), [("prefix", "attr1"), "val1"]) in
  print_endline (pp_to_string stanza);
  [%expect {|
    <prefix:name
      prefix:attr1='val1'/> |}]
;;

let%expect_test "create tag with attrs" =
  let tag = ("prefix", "name"), [("prefix", "attr1"), "val1"] in
  print_endline (pp_tag_to_string tag);
  [%expect {|
    prefix:name
      prefix:attr1='val1' |}]
;;

let%expect_test "create stanza with children" =
  let children = [create (("prefix", "name"), [("prefix", "attr1"), "val1"])] in
  let stanza = create ~children (("prefix", "name"), [("prefix", "attr1"), "val1"]) in
  print_endline (pp_to_string stanza);
  [%expect
    {|
      <prefix:name
        prefix:attr1='val1'>
        <prefix:name
        prefix:attr1='val1'/>
      </prefix:name> |}]
;;

let%expect_test "text stanza" =
  let text = Text ["text stanza"] in
  print_endline (pp_to_string text);
  [%expect {| text stanza |}]
;;

let%expect_test "multi-line text stanza" =
  let text = Text ["text stanza"; "line 2"; "line 3"] in
  print_endline (pp_to_string text);
  [%expect {|
    text stanza
    line 2
    line 3 |}]
;;
