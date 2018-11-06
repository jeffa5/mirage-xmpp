open Astring

(* a JID is of the form user@domain/resource *)
type t = string * string * string

let full _ = false

let to_string (user, domain, resource) =
  user ^ "@" ^ domain ^ if resource <> "" then "/" ^ resource else ""
;;

let of_string string =
  match String.cut ~sep:"@" string with
  | Some (user, domres) ->
    (match String.cut ~sep:"/" domres with
    | Some (domain, resource) -> user, domain, resource
    | None -> user, domres, "")
  | None -> assert false
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
