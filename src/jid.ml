open Astring
open Sexplib.Std

(* a JID is of the form user@domain/resource *)
type bare_jid = string * string [@@deriving sexp]
type full_jid = bare_jid * string [@@deriving sexp]

type t =
  | Full_JID of full_jid
  | Bare_JID of bare_jid
  | Domain of string
[@@deriving sexp]

let at_least_bare = function Full_JID _ | Bare_JID _ -> true | _ -> false

let to_bare = function
  | Full_JID (bare_jid, _) -> Bare_JID bare_jid
  | Bare_JID bare_jid -> Bare_JID bare_jid
  | _ -> raise Not_found
;;

let of_string str =
  match String.cut ~sep:"@" str with
  | Some (user, domres) ->
    (match String.cut ~sep:"/" domres with
    | Some (domain, resource) -> Full_JID ((user, domain), resource)
    | None -> Bare_JID (user, domres))
  | None -> Domain str
;;

let compare jid1 jid2 =
  let user1, domain1, resource1 =
    match jid1 with
    | Full_JID ((u1, d1), r1) -> u1, d1, r1
    | Bare_JID (u1, d1) -> u1, d1, ""
    | Domain d1 -> "", d1, ""
  in
  let user2, domain2, resource2 =
    match jid2 with
    | Full_JID ((u2, d2), r2) -> u2, d2, r2
    | Bare_JID (u2, d2) -> u2, d2, ""
    | Domain d2 -> "", d2, ""
  in
  let d = String.compare domain1 domain2 in
  let u = String.compare user1 user2 in
  let r = String.compare resource1 resource2 in
  if d = 0 then if u = 0 then r else u else d
;;

let create_resource () = Uuidm.(to_string (create `V4))

let set_resource new_resource = function
  | Full_JID ((user, domain), _resource) -> Full_JID ((user, domain), new_resource)
  | Bare_JID (user, domain) -> Full_JID ((user, domain), new_resource)
  | Domain _domain -> assert false
;;

let to_string = function
  | Full_JID ((user, domain), resource) -> user ^ "@" ^ domain ^ "/" ^ resource
  | Bare_JID (user, domain) -> user ^ "@" ^ domain
  | Domain domain -> domain
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
