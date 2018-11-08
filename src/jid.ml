open Astring

(* a JID is of the form user@domain/resource *)
type t = string * string * string

let full _ = false
let empty = "", "", ""

let of_string string =
  match String.cut ~sep:"@" string with
  | Some (user, domres) ->
    (match String.cut ~sep:"/" domres with
    | Some (domain, resource) -> user, domain, resource
    | None -> user, domres, "")
  | None -> assert false
;;

let compare (u1, d1, r1) (u2, d2, r2) =
  let d = String.compare d1 d2 in
  let u = String.compare u1 u2 in
  let r = String.compare r1 r2 in
  if d = 0 then if u = 0 then r else u else d
;;

let equal (u1, d1, r1) (u2, d2, r2) = u1 = u2 && d1 = d2 && r1 = r2

let to_string (user, domain, resource) =
  if equal empty (user, domain, resource)
  then "empty"
  else user ^ "@" ^ domain ^ if resource <> "" then "/" ^ resource else ""
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
