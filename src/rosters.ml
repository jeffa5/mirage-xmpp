(* Need to support user -> (user, name, group) *)

open Asetmap
module Jid_map = Map.Make (Jid)

type item =
  { handle : string
  ; subscribed : string
  ; groups : string list }

type roster = item Jid_map.t

type user =
  { presence_status : bool
  ; roster : roster }

let t = ref Jid_map.empty

let create_roster ~jid =
  t := Jid_map.add jid {presence_status = false; roster = Jid_map.empty} !t
;;

let create_item ~handle ~subscribed ~groups () = {handle; subscribed; groups}

let rec set_item ~user_jid ~target_jid ~handle ~subscribed ~groups =
  let item = create_item ~handle ~subscribed ~groups () in
  match Jid_map.find user_jid !t with
  | Some {presence_status; roster} ->
    t :=
      Jid_map.add
        user_jid
        {presence_status; roster = Jid_map.add target_jid item roster}
        !t
  | None ->
    create_roster ~jid:user_jid;
    set_item ~user_jid ~target_jid ~handle ~subscribed ~groups
;;

let get user_jid =
  match Jid_map.find user_jid !t with
  | Some {roster; _} ->
    Jid_map.to_list roster
    |> List.map (fun (jid, {handle; subscribed; groups}) ->
           jid, handle, subscribed, groups )
  | None -> []
;;

let groups_to_string groups = "[" ^ String.concat "," groups ^ "]"

let item_to_string {handle; subscribed; groups} =
  "{" ^ String.concat "; " [handle; subscribed; groups_to_string groups] ^ "}"
;;

let roster_to_string roster =
  Jid_map.to_list roster
  |> List.map (fun (jid, item) -> Jid.to_string jid ^ ": " ^ item_to_string item)
  |> String.concat "\n"
;;

let user_to_string {presence_status; roster} =
  string_of_bool presence_status ^ "; " ^ roster_to_string roster
;;

let to_string () =
  "["
  ^ ( Jid_map.to_list !t
    |> List.map (fun (jid, user) -> Jid.to_string jid ^ ": " ^ user_to_string user)
    |> String.concat "\n" )
  ^ "]"
;;

let clear () = t := Jid_map.empty

let%expect_test "empty initially" =
  clear ();
  print_endline (to_string ());
  [%expect {| [] |}]
;;

let%expect_test "add one jid" =
  clear ();
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscribed:"none"
    ~groups:[];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: false; romeo@im.example.com: {my romeo; none; []}] |}]
;;

let%expect_test "add one jid with groups" =
  clear ();
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscribed:"none"
    ~groups:["Group1"; "Group2"];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: false; romeo@im.example.com: {my romeo; none; [Group1,Group2]}] |}]
;;

let%expect_test "add same jid multiple times" =
  clear ();
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscribed:"none"
    ~groups:["Group1"; "Group2"];
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscribed:"none"
    ~groups:["Group1"; "Group2"];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: false; romeo@im.example.com: {my romeo; none; [Group1,Group2]}] |}]
;;

let%expect_test "add same jid multiple times" =
  clear ();
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscribed:"none"
    ~groups:["Group1"; "Group2"];
  set_item
    ~user_jid:(Jid.of_string "lord@im.example.com")
    ~target_jid:(Jid.of_string "king@im.example.com")
    ~handle:"king1"
    ~subscribed:"none"
    ~groups:["Kings1"; "Rulers"; "Others"];
  print_endline (to_string ());
  [%expect
    {|
      [juliet@im.example.com: false; romeo@im.example.com: {my romeo; none; [Group1,Group2]}
      lord@im.example.com: false; king@im.example.com: {king1; none; [Kings1,Rulers,Others]}] |}]
;;
