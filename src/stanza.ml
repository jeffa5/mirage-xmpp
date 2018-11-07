type name = string * string
type attribute = name * string
type tag = name * attribute list

type t =
  | Stanza of tag * t list
  | Text of string

let create ?(children = []) tag = Stanza (tag, children)

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

let name_to_string (prefix, name) = if prefix <> "" then prefix ^ ":" ^ name else name

let attribute_to_string (name, attribute) =
  let name_string = name_to_string name in
  name_string ^ "=" ^ attribute
;;

let tag_to_string (name, attributes) =
  let attr_string =
    Astring.String.concat ~sep:" " (List.map (fun a -> attribute_to_string a) attributes)
  in
  let name_string = name_to_string name in
  name_string ^ if attr_string <> "" then " " ^ attr_string else ""
;;

let rec to_string = function
  | Stanza ((name, attrs), l) ->
    let tag_string = tag_to_string (name, attrs) in
    (match l with
    | [] -> "<" ^ tag_string ^ " />"
    | ss ->
      let stanzas =
        Astring.String.concat ~sep:"\n" (List.map (fun s -> to_string s) ss)
      in
      "<" ^ tag_string ^ ">" ^ stanzas ^ "</" ^ name_to_string name ^ ">")
  | Text s -> s
;;

let%expect_test "empty tag prefix" =
  let tag = ("", "name"), [] in
  print_endline (tag_to_string tag);
  [%expect {| name |}]
;;

let%expect_test "empty prefix in stanza" =
  let stanza = Stanza ((("", "name"), []), []) in
  print_endline (to_string stanza);
  [%expect {| <name /> |}]
;;

let%expect_test "create" =
  let stanza = create (("prefix", "name"), []) in
  print_endline (to_string stanza);
  [%expect {| <prefix:name /> |}]
;;

let%expect_test "create tag" =
  let tag = ("prefix", "name"), [] in
  print_endline (tag_to_string tag);
  [%expect {| prefix:name |}]
;;

let%expect_test "empty tag prefix with attrs" =
  let tag = ("", "name"), [("", "attr1"), "val1"] in
  print_endline (tag_to_string tag);
  [%expect {| name attr1=val1 |}]
;;

let%expect_test "empty prefix in stanza with attrs" =
  let stanza = Stanza ((("", "name"), [("", "attr1"), "val1"]), []) in
  print_endline (to_string stanza);
  [%expect {| <name attr1=val1 /> |}]
;;

let%expect_test "create stanza with attrs" =
  let stanza = create (("prefix", "name"), [("prefix", "attr1"), "val1"]) in
  print_endline (to_string stanza);
  [%expect {| <prefix:name prefix:attr1=val1 /> |}]
;;

let%expect_test "create tag with attrs" =
  let tag = ("prefix", "name"), [("prefix", "attr1"), "val1"] in
  print_endline (tag_to_string tag);
  [%expect {| prefix:name prefix:attr1=val1 |}]
;;

let%expect_test "create stanza with children" =
  let children = [create (("prefix", "name"), [("prefix", "attr1"), "val1"])] in
  let stanza = create ~children (("prefix", "name"), [("prefix", "attr1"), "val1"]) in
  print_endline (to_string stanza);
  [%expect
    {| <prefix:name prefix:attr1=val1><prefix:name prefix:attr1=val1 /></prefix:name> |}]
;;

let%expect_test "text stanza" =
  let text = Text "text stanza" in
  print_endline (to_string text);
  [%expect {| text stanza |}]
;;
