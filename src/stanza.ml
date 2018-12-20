open Astring

type t =
  | Message of Xml.t
  | Presence of Xml.t
  | Iq of Xml.t

let create_message ?(children = []) attributes =
  Message (Element ((("", "message"), attributes), children))
;;

let create_presence ?(children = []) attributes =
  Presence (Element ((("", "presence"), attributes), children))
;;

let create_iq ?(children = []) attributes =
  Iq (Element ((("", "iq"), attributes), children))
;;

let create_iq_bind ?(children = []) id =
  create_iq
    ["", Xml.Id id; "", Xml.Type "result"]
    ~children:
      [ Xml.create
          (("", "bind"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-bind"])
          ~children ]
;;

let create_iq_query ?(children = []) ~id ~from () =
  create_iq
    ["", Xml.Id id; "", Xml.To from; "", Xml.Type "result"]
    ~children:
      [ Xml.create
          (("", "query"), ["", Xml.Xmlns "jabber:iq:roster"; "", Xml.Ver "ver7"])
          ~children ]
;;

let rec get_id = function
  | [] -> raise Not_found
  | (_, Xml.Id id) :: _ -> id
  | _ :: attrs -> get_id attrs
;;

let rec get_from = function
  | [] -> raise Not_found
  | (_, Xml.From jid) :: _ -> jid
  | _ :: attrs -> get_from attrs
;;

let rec get_to = function
  | [] -> raise Not_found
  | (_, Xml.To jid) :: _ -> jid
  | _ :: attrs -> get_to attrs
;;

let rec get_type = function
  | [] -> raise Not_found
  | (_, Xml.Type t) :: _ -> t
  | _ :: attrs -> get_type attrs
;;

let rec get_version = function
  | [] -> raise Not_found
  | (_, Xml.Version v) :: _ -> v
  | _ :: attrs -> get_version attrs
;;

let rec get_jid = function
  | [] -> raise Not_found
  | (_, Xml.Jid jid) :: _ -> jid
  | _ :: attrs -> get_jid attrs
;;

let rec get_name = function
  | [] -> raise Not_found
  | (_, Xml.Name name) :: _ -> name
  | _ :: attrs -> get_name attrs
;;

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
