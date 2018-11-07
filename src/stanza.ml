open Astring

type name = string * string
type attribute = name * string
type tag = name * attribute list

type t =
  | Stanza of tag * t list
  | Text of string list

let create ?(children = []) tag = Stanza (tag, children)
let text s = Text s

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
    String.concat ~sep:" " (List.map (fun a -> attribute_to_string a) attributes)
  in
  let name_string = name_to_string name in
  name_string ^ if attr_string <> "" then " " ^ attr_string else ""
;;

let to_string s =
  let rec aux s depth =
    let indent =
      String.v ~len:(depth * 2) (fun _ ->
          match String.to_char " " with Some c -> c | None -> assert false )
    in
    match s with
    | Stanza ((name, attrs), l) ->
      let tag_string = tag_to_string (name, attrs) in
      (match l with
      | [] -> indent ^ "<" ^ tag_string ^ " />"
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
    {|
      <prefix:name prefix:attr1=val1>
        <prefix:name prefix:attr1=val1 />
      </prefix:name> |}]
;;

let%expect_test "text stanza" =
  let text = Text ["text stanza"] in
  print_endline (to_string text);
  [%expect {| text stanza |}]
;;

let%expect_test "multi-line text stanza" =
  let text = Text ["text stanza"; "line 2"; "line 3"] in
  print_endline (to_string text);
  [%expect {|
    text stanza
    line 2
    line 3 |}]
;;
