(* Need to support user -> (user, name, group) *)

open Asetmap
module Jid_map = Map.Make (Jid)

type availability =
  | Online
  | Offline

let availability_to_string = function Online -> "Online" | Offline -> "Offline"

type subscription =
  | None
  | From
  | Both

let subscription_to_string = function None -> "none" | From -> "from" | Both -> "both"

exception ConversionError

let subscription_of_string = function
  | "none" -> None
  | "from" -> From
  | "both" -> Both
  | _ -> raise ConversionError
;;

type item =
  { handle : string
  ; subscription : subscription
  ; groups : string list }

type roster = item Jid_map.t

type user =
  { presence_status : availability
  ; roster : roster }

let t = ref Jid_map.empty

let create_roster ~jid =
  t := Jid_map.add jid {presence_status = Offline; roster = Jid_map.empty} !t
;;

let set_presence ~jid updated_presence_status =
  match Jid_map.find jid !t with
  | Some {roster; _} ->
    t := Jid_map.add jid {presence_status = updated_presence_status; roster} !t
  | None ->
    t :=
      Jid_map.add
        jid
        {presence_status = updated_presence_status; roster = Jid_map.empty}
        !t
;;

let create_item ~handle ~subscription ~groups () = {handle; subscription; groups}

let rec set_item ~user_jid ~target_jid ~handle ~subscription ~groups =
  let item = create_item ~handle ~subscription ~groups () in
  match Jid_map.find user_jid !t with
  | Some {presence_status; roster} ->
    t :=
      Jid_map.add
        user_jid
        {presence_status; roster = Jid_map.add target_jid item roster}
        !t
  | None ->
    create_roster ~jid:user_jid;
    set_item ~user_jid ~target_jid ~handle ~subscription ~groups
;;

let get user_jid =
  match Jid_map.find user_jid !t with
  | Some {roster; _} ->
    Jid_map.to_list roster
    |> List.map (fun (jid, {handle; subscription; groups}) ->
           jid, handle, subscription, groups )
  | None -> []
;;

let get_subscribers user_jid =
  match Jid_map.find user_jid !t with
  | Some {roster; _} ->
    Jid_map.to_list roster
    |> List.filter (fun (_jid, {subscription; _}) ->
           match subscription with From | Both -> true | _ -> false )
    |> List.map (fun (jid, _) -> jid)
  | None -> []
;;

let groups_to_string groups = "[" ^ String.concat "," groups ^ "]"

let item_to_string {handle; subscription; groups} =
  "{"
  ^ String.concat
      "; "
      [handle; subscription_to_string subscription; groups_to_string groups]
  ^ "}"
;;

let roster_to_string roster =
  Jid_map.to_list roster
  |> List.map (fun (jid, item) -> Jid.to_string jid ^ ": " ^ item_to_string item)
  |> String.concat "\n"
;;

let user_to_string {presence_status; roster} =
  availability_to_string presence_status ^ "; " ^ roster_to_string roster
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
    ~subscription:None
    ~groups:[];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; romeo@im.example.com: {my romeo; none; []}] |}]
;;

let%expect_test "add one jid with groups" =
  clear ();
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscription:None
    ~groups:["Group1"; "Group2"];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; romeo@im.example.com: {my romeo; none; [Group1,Group2]}] |}]
;;

let%expect_test "add same jid multiple times" =
  clear ();
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscription:None
    ~groups:["Group1"; "Group2"];
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscription:None
    ~groups:["Group1"; "Group2"];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; romeo@im.example.com: {my romeo; none; [Group1,Group2]}] |}]
;;

let%expect_test "add same jid multiple times" =
  clear ();
  set_item
    ~user_jid:(Jid.of_string "juliet@im.example.com")
    ~target_jid:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~subscription:None
    ~groups:["Group1"; "Group2"];
  set_item
    ~user_jid:(Jid.of_string "lord@im.example.com")
    ~target_jid:(Jid.of_string "king@im.example.com")
    ~handle:"king1"
    ~subscription:None
    ~groups:["Kings1"; "Rulers"; "Others"];
  print_endline (to_string ());
  [%expect
    {|
      [juliet@im.example.com: Offline; romeo@im.example.com: {my romeo; none; [Group1,Group2]}
      lord@im.example.com: Offline; king@im.example.com: {king1; none; [Kings1,Rulers,Others]}] |}]
;;
