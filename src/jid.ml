open Astring
open Sexplib.Std

exception MalformedJID of string

module Bare = struct
  type t = string * string [@@deriving sexp, ord]

  let set_resource resource bare_jid = bare_jid, resource

  exception MalformedBareJID of string

  let of_string str =
    match String.cut ~sep:"@" str with
    | Some (user, domres) ->
      (match String.cut ~sep:"/" domres with
      | Some _ -> raise @@ MalformedBareJID str
      | None -> user, domres)
    | None -> raise @@ MalformedJID str
  ;;
end

module Full = struct
  type t = (string * string) * string [@@deriving sexp, ord]

  let to_bare (bare_jid, _) = bare_jid
  let set_resource resource (bare_jid, _) = bare_jid, resource

  exception MalformedFullJID of string

  let of_string str =
    match String.cut ~sep:"@" str with
    | Some (user, domres) ->
      (match String.cut ~sep:"/" domres with
      | Some (domain, resource) -> (user, domain), resource
      | None -> raise @@ MalformedFullJID str)
    | None -> raise @@ MalformedJID str
  ;;

  let to_string ((user, domain), resource) = user ^ "@" ^ domain ^ "/" ^ resource
end

module Domain = struct
  type t = string [@@deriving sexp]
end

type t =
  | Full_JID of Full.t
  | Bare_JID of Bare.t
  | Domain of Domain.t
[@@deriving sexp]

let to_bare_raw = function
  | Full_JID (bare_jid, _) -> bare_jid
  | Bare_JID bare_jid -> bare_jid
  | Domain dom ->
    raise @@ MalformedJID ("Not allowed to convert a Domain to a raw jid: " ^ dom)
;;

let to_bare = function
  | Full_JID (bare_jid, _) -> Bare_JID bare_jid
  | Bare_JID bare_jid -> Bare_JID bare_jid
  | Domain dom ->
    raise @@ MalformedJID ("Not allowed to convert a Domain to a raw jid: " ^ dom)
;;

let anon () = "anon-" ^ Uuidm.(to_string (create `V4))

let of_string str =
  match String.cut ~sep:"@" str with
  | Some (user, domres) ->
    (match String.cut ~sep:"/" domres with
    | Some (domain, resource) -> Full_JID ((user, domain), resource)
    | None -> Bare_JID (user, domres))
  | None -> Domain str
;;

let create_resource () = Uuidm.(to_string (create `V4))

let set_resource resource = function
  | Full_JID fjid -> Full_JID (Full.set_resource resource fjid)
  | Bare_JID bjid -> Full_JID (Bare.set_resource resource bjid)
  | Domain dom ->
    raise @@ MalformedJID ("Not allowed to set resource on a Domain: " ^ dom)
;;

let to_string = function
  | Full_JID ((user, domain), resource) -> user ^ "@" ^ domain ^ "/" ^ resource
  | Bare_JID (user, domain) -> user ^ "@" ^ domain
  | Domain dom -> dom
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
