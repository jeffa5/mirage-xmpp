(* a JID is of the form user@domain/resource *)
type t = string * string * string

let full _ = false

let to_string (user, domain, resource) =
  user ^ "@" ^ domain ^ if resource <> "" then "/" ^ resource else ""
;;

let of_string string =
  let regex_at = Str.regexp_string "@" in
  let regex_slash = Str.regexp_string "/" in
  match Str.split regex_at string with
  | [user; domres] ->
    (match Str.split regex_slash domres with
    | [domain; resource] -> user, domain, resource
    | [domain] -> user, domain, ""
    | _ -> assert false)
  | _ -> assert false
;;

let%expect_test "make jid" =
  let jid = of_string "user@domain/resource" in
  print_endline (to_string jid);
  [%expect {| user@domain/resource |}]
;;

let%expect_test "no resource in jid" =
  let jid = of_string "user@domain" in
  print_endline (to_string jid);
  [%expect {| user@domain |}]
;;
