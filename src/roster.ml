(* Need to support user -> (user, name, group) *)
open Asetmap
module Jid_map = Map.Make (Jid)

type contact =
  { nickname : string
  ; group : string
  ; presence : bool }

type t = contact Jid_map.t

let empty = Jid_map.empty

let set_presence t jid presence =
  let contact = Jid_map.get jid t in
  let new_contact = {contact with presence} in
  Jid_map.add jid new_contact t
;;

let get_presence t jid =
  let contact = Jid_map.get jid t in
  contact.presence
;;

let set_contact t jid contact = Jid_map.add jid contact t
let get_contact t jid = Jid_map.get jid t
let get_jids t = List.map (fun (j, _) -> j) (Jid_map.to_list t)

let contact_to_string contact =
  "{\n"
  ^ contact.nickname
  ^ ",\n"
  ^ contact.group
  ^ ",\n"
  ^ string_of_bool contact.presence
  ^ "\n}"
;;

let to_string t =
  "["
  ^ ( Jid_map.to_list t
    |> List.map (fun (k, v) -> Jid.to_string k ^ contact_to_string v)
    |> String.concat "," )
  ^ "]"
;;
