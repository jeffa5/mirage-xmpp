open Asetmap
module Table = Map.Make (Jid)

type t = (Actions.t option -> unit) Table.t

let empty = Table.empty
let add jid f t = Table.add jid f t
let find jid t = Table.find jid t
let remove jid t = Table.remove jid t

let to_string t =
  let aux (jid, _) = Jid.to_string jid in
  String.concat "\n" (List.map (fun p -> aux p) (Table.to_list t))
;;
