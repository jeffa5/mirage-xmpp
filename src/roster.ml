type t = (Jid.t * bool) list

let empty = []

let update t jid presence =
  let rec aux = function
    | [] -> [jid, presence]
    | (j, p) :: ss -> if Jid.equal jid j then (j, presence) :: ss else (j, p) :: aux ss
  in
  aux t
;;

let delete t jid =
  let rec aux = function
    | [] -> raise Not_found
    | (j, p) :: ss -> if Jid.equal jid j then ss else (j, p) :: aux ss
  in
  aux t
;;

let to_string t =
  let aux (j, p) = Jid.to_string j ^ ": " ^ string_of_bool p in
  "[" ^ String.concat "; " (List.map (fun item -> aux item) t) ^ "]"
;;
