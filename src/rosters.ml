open Asetmap
module Jid_map = Map.Make (Jid)

type availability =
  | Online
  | Offline

let availability_to_string = function Online -> "Online" | Offline -> "Offline"

type subscription =
  | None
  | To
  | From
  | Both

let subscription_to_string = function
  | None -> "none"
  | To -> "to"
  | From -> "from"
  | Both -> "both"
;;

exception ConversionError

let subscription_of_string = function
  | "none" -> None
  | "to" -> To
  | "from" -> From
  | "both" -> Both
  | _ -> raise ConversionError
;;

type item =
  { handle : string
  ; subscription : subscription
  ; ask : bool
  ; groups : string list }

let item_to_tuple {handle; subscription; ask; groups} = handle, subscription, ask, groups

type roster = item Jid_map.t

type user =
  { presence_status : availability
  ; roster : roster }

let t = ref Jid_map.empty

let set_presence ~jid updated_presence_status =
  let jid = Jid.to_bare jid in
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

let remove_item user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    t := Jid_map.add user {r with roster = Jid_map.remove contact roster} !t
  | None -> ()
;;

let upgrade_subscription_to user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    (match Jid_map.find contact roster with
    | Some ({subscription; _} as item) ->
      let new_subscription =
        match subscription with None -> To | To -> To | From -> Both | Both -> Both
      in
      let new_roster =
        Jid_map.add contact {item with subscription = new_subscription} roster
      in
      t := Jid_map.add user {r with roster = new_roster} !t
    | None -> raise Not_found)
  | None -> raise Not_found
;;

let upgrade_subscription_from user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    (match Jid_map.find contact roster with
    | Some ({subscription; _} as item) ->
      let new_subscription =
        match subscription with None -> From | To -> Both | From -> From | Both -> Both
      in
      let new_roster =
        Jid_map.add contact {item with subscription = new_subscription} roster
      in
      t := Jid_map.add user {r with roster = new_roster} !t
    | None -> raise Not_found)
  | None -> raise Not_found
;;

let create_item ~handle ~subscription ?(ask = false) ~groups () =
  {handle; subscription; ask; groups}
;;

let set_item ~user ~contact ~handle ~groups =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    let new_item =
      match Jid_map.find contact roster with
      | Some item -> {item with handle; groups}
      | None -> create_item ~handle ~subscription:None ~groups ()
    in
    let new_roster = Jid_map.add contact new_item roster in
    t := Jid_map.add user {r with roster = new_roster} !t
  | None ->
    let new_roster =
      Jid_map.add
        contact
        (create_item ~handle ~subscription:None ~groups ())
        Jid_map.empty
    in
    t := Jid_map.add user {presence_status = Offline; roster = new_roster} !t
;;

let get_presence user =
  let user = Jid.to_bare user in
  match Jid_map.find user !t with
  | Some {presence_status; _} -> presence_status
  | None -> Offline
;;

let get_roster_item user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some {roster; _} ->
    (match Jid_map.find contact roster with Some item -> Some item | None -> None)
  | None -> None
;;

let get_roster_items user =
  let user = Jid.to_bare user in
  match Jid_map.find user !t with
  | Some {roster; _} -> Jid_map.to_list roster
  | None -> []
;;

let get_ask user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some {roster; _} ->
    (match Jid_map.find contact roster with
    | Some {ask; _} -> ask
    | None -> raise Not_found)
  | None -> raise Not_found
;;

let get_subscription user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some {roster; _} ->
    (match Jid_map.find contact roster with
    | Some {subscription; _} -> Some subscription
    | None -> None)
  | None -> None
;;

let get_subscribers user =
  let user = Jid.to_bare user in
  match Jid_map.find user !t with
  | Some {roster; _} ->
    Jid_map.to_list roster
    |> List.filter (fun (_jid, {subscription; _}) ->
           match subscription with From | Both -> true | _ -> false )
    |> List.map (fun (jid, _) -> jid)
  | None -> []
;;

let update_ask user contact value =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    (match Jid_map.find contact roster with
    | Some item ->
      t :=
        Jid_map.add
          user
          {r with roster = Jid_map.add contact {item with ask = value} roster}
          !t
    | None -> ())
  | None -> ()
;;

let unset_ask user contact = update_ask user contact false
let set_ask user contact = update_ask user contact true
let groups_to_string groups = "[" ^ String.concat "," groups ^ "]"

let item_to_string {handle; subscription; ask; groups} =
  "{"
  ^ String.concat
      "; "
      [ handle
      ; subscription_to_string subscription
      ; string_of_bool ask
      ; groups_to_string groups ]
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
    ~user:(Jid.of_string "juliet@im.example.com")
    ~contact:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~groups:[];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; romeo@im.example.com: {my romeo; none; false; []}] |}]
;;

let%expect_test "add one jid with groups" =
  clear ();
  set_item
    ~user:(Jid.of_string "juliet@im.example.com")
    ~contact:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; romeo@im.example.com: {my romeo; none; false; [Group1,Group2]}] |}]
;;

let%expect_test "add same jid multiple times" =
  clear ();
  set_item
    ~user:(Jid.of_string "juliet@im.example.com")
    ~contact:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"];
  set_item
    ~user:(Jid.of_string "juliet@im.example.com")
    ~contact:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"];
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; romeo@im.example.com: {my romeo; none; false; [Group1,Group2]}] |}]
;;

let%expect_test "add same jid multiple times" =
  clear ();
  set_item
    ~user:(Jid.of_string "juliet@im.example.com")
    ~contact:(Jid.of_string "romeo@im.example.com")
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"];
  set_item
    ~user:(Jid.of_string "lord@im.example.com")
    ~contact:(Jid.of_string "king@im.example.com")
    ~handle:"king1"
    ~groups:["Kings1"; "Rulers"; "Others"];
  print_endline (to_string ());
  [%expect
    {|
      [juliet@im.example.com: Offline; romeo@im.example.com: {my romeo; none; false; [Group1,Group2]}
      lord@im.example.com: Offline; king@im.example.com: {king1; none; false; [Kings1,Rulers,Others]}] |}]
;;
