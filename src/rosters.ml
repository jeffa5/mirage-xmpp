open Asetmap
module Jid_map = Map.Make (Jid)

type subscription =
  | None
  | To
  | From
  | Both
  | Remove
[@@deriving sexp]

type presence =
  | Online
  | Offline
[@@deriving sexp]

type item =
  { handle : string
  ; subscription : subscription
  ; ask : bool
  ; groups : string list }

type roster = item Jid_map.t

type user =
  { presence_status : presence
  ; roster : roster }

let t = ref Jid_map.empty
let presence_to_string = function Online -> "Online" | Offline -> "Offline"

let subscription_to_string = function
  | None -> "none"
  | To -> "to"
  | From -> "from"
  | Both -> "both"
  | Remove -> "remove"
;;

let groups_to_string groups = "[" ^ String.concat ", " groups ^ "]"
let item_to_tuple {handle; subscription; ask; groups} = handle, subscription, ask, groups

let item_to_string {handle; subscription; ask; groups} =
  "{"
  ^ String.concat
      "; "
      [ "handle=" ^ handle
      ; "subscription=" ^ subscription_to_string subscription
      ; "ask=" ^ string_of_bool ask
      ; "groups=" ^ groups_to_string groups ]
  ^ "}"
;;

let roster_to_string roster =
  "["
  ^ ( Jid_map.to_list roster
    |> List.map (fun (jid, item) -> Jid.to_string jid ^ ": " ^ item_to_string item)
    |> String.concat ";\n" )
  ^ "]"
;;

let user_to_string {presence_status; roster} =
  presence_to_string presence_status ^ "; " ^ roster_to_string roster
;;

let to_string () =
  "["
  ^ ( Jid_map.to_list !t
    |> List.map (fun (jid, user) -> Jid.to_string jid ^ ": " ^ user_to_string user)
    |> String.concat "\n" )
  ^ "]"
;;

let set_presence jid updated_presence_status =
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

let downgrade_subscription_from user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    (match Jid_map.find contact roster with
    | Some ({subscription; _} as item) ->
      let new_subscription =
        match subscription with
        | None -> None
        | To -> To
        | From -> None
        | Both -> To
        | Remove -> Remove
      in
      let new_roster =
        Jid_map.add contact {item with subscription = new_subscription} roster
      in
      t := Jid_map.add user {r with roster = new_roster} !t
    | None -> ())
  | None -> ()
;;

let downgrade_subscription_to user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    (match Jid_map.find contact roster with
    | Some ({subscription; _} as item) ->
      let new_subscription =
        match subscription with
        | None -> None
        | To -> None
        | From -> From
        | Both -> From
        | Remove -> Remove
      in
      let new_roster =
        Jid_map.add contact {item with subscription = new_subscription} roster
      in
      t := Jid_map.add user {r with roster = new_roster} !t
    | None -> ())
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
        match subscription with
        | None -> To
        | To -> To
        | From -> Both
        | Both -> Both
        | Remove -> Remove
      in
      let new_roster =
        Jid_map.add contact {item with subscription = new_subscription} roster
      in
      t := Jid_map.add user {r with roster = new_roster} !t
    | None -> ())
  | None -> ()
;;

let upgrade_subscription_from user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    (match Jid_map.find contact roster with
    | Some ({subscription; _} as item) ->
      let new_subscription =
        match subscription with
        | None -> From
        | To -> Both
        | From -> From
        | Both -> Both
        | Remove -> Remove
      in
      let new_roster =
        Jid_map.add contact {item with subscription = new_subscription} roster
      in
      t := Jid_map.add user {r with roster = new_roster} !t
    | None -> ())
  | None -> ()
;;

let create_item ~handle ~subscription ?(ask = false) ~groups () =
  {handle; subscription; ask; groups}
;;

let set_item ?(subscription = None) ?(handle = "") ?(groups = []) user contact =
  let user = Jid.to_bare user in
  let contact = Jid.to_bare contact in
  match Jid_map.find user !t with
  | Some ({roster; _} as r) ->
    let new_item =
      match Jid_map.find contact roster with
      | Some item -> {item with handle; groups}
      | None -> create_item ~handle ~subscription ~groups ()
    in
    let new_roster = Jid_map.add contact new_item roster in
    t := Jid_map.add user {r with roster = new_roster} !t
  | None ->
    let new_roster =
      Jid_map.add contact (create_item ~handle ~subscription ~groups ()) Jid_map.empty
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
    (match Jid_map.find contact roster with Some {ask; _} -> Some ask | None -> None)
  | None -> None
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

let get_subscriptions user =
  let user = Jid.to_bare user in
  match Jid_map.find user !t with
  | Some {roster; _} ->
    Jid_map.to_list roster
    |> List.filter (fun (_jid, {subscription; _}) ->
           match subscription with To | Both -> true | _ -> false )
    |> List.map (fun (jid, _) -> jid)
  | None -> []
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
let clear () = t := Jid_map.empty

let%expect_test "empty initially" =
  clear ();
  print_endline (to_string ());
  [%expect {| [] |}]
;;

let%expect_test "add one jid" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:[]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[]}]] |}]
;;

let%expect_test "add one jid with groups" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "add same jid multiple times" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {|
      [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "add same jid multiple times with different groups" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  set_item
    ~handle:"king1"
    ~groups:["Kings1"; "Rulers"; "Others"]
    (Jid.of_string "lord@im.example.com")
    (Jid.of_string "king@im.example.com");
  print_endline (to_string ());
  [%expect
    {|
      [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]
      lord@im.example.com: Offline; [king@im.example.com: {handle=king1; subscription=none; ask=false; groups=[Kings1, Rulers, Others]}]] |}]
;;

let%expect_test "add two different jids to the roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "romeo@im.example.com");
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "other@im.example.com");
  print_endline (to_string ());
  [%expect
    {|
      [juliet@im.example.com: Offline; [other@im.example.com: {handle=; subscription=none; ask=false; groups=[]};
      romeo@im.example.com: {handle=; subscription=none; ask=false; groups=[]}]] |}]
;;

let%expect_test "removing an item" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {|
    [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}];
  remove_item
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect {|
    [juliet@im.example.com: Offline; []] |}]
;;

let%expect_test "removing a non-existant item" =
  clear ();
  remove_item
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect {|
    [] |}]
;;

let%expect_test "downgrade subscription from when no items in roster" =
  clear ();
  downgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect {| [] |}]
;;

let%expect_test "downgrade subscription from when no valid item in roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "other@im.example.com");
  downgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [other@im.example.com: {handle=; subscription=none; ask=false; groups=[]}]] |}]
;;

let%expect_test "downgrade subscription from with None item" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}];
  downgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "downgrade subscription from with To item" =
  clear ();
  set_item
    ~subscription:To
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=to; ask=false; groups=[Group1, Group2]}]] |}];
  downgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=to; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "downgrade subscription from with From item" =
  clear ();
  set_item
    ~subscription:From
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=from; ask=false; groups=[Group1, Group2]}]] |}];
  downgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "downgrade subscription from with Both item" =
  clear ();
  set_item
    ~subscription:Both
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=both; ask=false; groups=[Group1, Group2]}]] |}];
  downgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=to; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "downgrade subscription to when no items in roster" =
  clear ();
  downgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect {| [] |}]
;;

let%expect_test "downgrade subscription to when no valid item in roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "other@im.example.com");
  downgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [other@im.example.com: {handle=; subscription=none; ask=false; groups=[]}]] |}]
;;

let%expect_test "downgrade subscription to with None item" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}];
  downgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "downgrade subscription to with To item" =
  clear ();
  set_item
    ~subscription:To
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=to; ask=false; groups=[Group1, Group2]}]] |}];
  downgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "downgrade subscription to with From item" =
  clear ();
  set_item
    ~subscription:From
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=from; ask=false; groups=[Group1, Group2]}]] |}];
  downgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=from; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "downgrade subscription to with Both item" =
  clear ();
  set_item
    ~subscription:Both
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=both; ask=false; groups=[Group1, Group2]}]] |}];
  downgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=from; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "upgrade subscription to when no items in roster" =
  clear ();
  upgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect {| [] |}]
;;

let%expect_test "upgrade subscription to when no valid item in roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "other@im.example.com");
  upgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [other@im.example.com: {handle=; subscription=none; ask=false; groups=[]}]] |}]
;;

let%expect_test "upgrade subscription to with None item" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}];
  upgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=to; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "upgrade subscription to with To item" =
  clear ();
  set_item
    ~subscription:To
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=to; ask=false; groups=[Group1, Group2]}]] |}];
  upgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=to; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "upgrade subscription to with From item" =
  clear ();
  set_item
    ~subscription:From
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=from; ask=false; groups=[Group1, Group2]}]] |}];
  upgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=both; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "upgrade subscription to with Both item" =
  clear ();
  set_item
    ~subscription:Both
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=both; ask=false; groups=[Group1, Group2]}]] |}];
  upgrade_subscription_to
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=both; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "upgrade subscription from when no items in roster" =
  clear ();
  upgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect {| [] |}]
;;

let%expect_test "upgrade subscription from when no valid item in roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "other@im.example.com");
  upgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [other@im.example.com: {handle=; subscription=none; ask=false; groups=[]}]] |}]
;;

let%expect_test "upgrade subscription from with None item" =
  clear ();
  set_item
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=none; ask=false; groups=[Group1, Group2]}]] |}];
  upgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=from; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "upgrade subscription from with To item" =
  clear ();
  set_item
    ~subscription:To
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=to; ask=false; groups=[Group1, Group2]}]] |}];
  upgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=both; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "upgrade subscription from with From item" =
  clear ();
  set_item
    ~subscription:From
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=from; ask=false; groups=[Group1, Group2]}]] |}];
  upgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=from; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "upgrade subscription from with Both item" =
  clear ();
  set_item
    ~subscription:Both
    ~handle:"my romeo"
    ~groups:["Group1"; "Group2"]
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=both; ask=false; groups=[Group1, Group2]}]] |}];
  upgrade_subscription_from
    (Jid.of_string "juliet@im.example.com")
    (Jid.of_string "romeo@im.example.com");
  print_endline (to_string ());
  [%expect
    {| [juliet@im.example.com: Offline; [romeo@im.example.com: {handle=my romeo; subscription=both; ask=false; groups=[Group1, Group2]}]] |}]
;;

let%expect_test "get ask on empty roster" =
  clear ();
  print_endline
  @@ Utils.option_to_string
       string_of_bool
       (get_ask
          (Jid.of_string "juliet@im.example.com")
          (Jid.of_string "romeo@im.example.com"));
  [%expect {| None |}]
;;

let%expect_test "get ask with user in roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "romeo@im.example.com");
  print_endline
  @@ Utils.option_to_string
       string_of_bool
       (get_ask
          (Jid.of_string "juliet@im.example.com")
          (Jid.of_string "romeo@im.example.com"));
  [%expect {| Some: false |}]
;;

let%expect_test "get ask with user not in roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "romeo@im.example.com");
  print_endline
  @@ Utils.option_to_string
       string_of_bool
       (get_ask
          (Jid.of_string "juliet@im.example.com")
          (Jid.of_string "other@im.example.com"));
  [%expect {| None |}]
;;

let%expect_test "get subscription with empty roster" =
  clear ();
  print_endline
  @@ Utils.option_to_string
       subscription_to_string
       (get_subscription
          (Jid.of_string "juliet@im.example.com")
          (Jid.of_string "romeo@im.example.com"));
  [%expect {| None |}]
;;

let%expect_test "get subscription with user in roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "romeo@im.example.com");
  print_endline
  @@ Utils.option_to_string
       subscription_to_string
       (get_subscription
          (Jid.of_string "juliet@im.example.com")
          (Jid.of_string "romeo@im.example.com"));
  [%expect {| Some: none |}]
;;

let%expect_test "get subscription with user not in roster" =
  clear ();
  set_item (Jid.of_string "juliet@im.example.com") (Jid.of_string "romeo@im.example.com");
  print_endline
  @@ Utils.option_to_string
       subscription_to_string
       (get_subscription
          (Jid.of_string "juliet@im.example.com")
          (Jid.of_string "other@im.example.com"));
  [%expect {| None |}]
;;
