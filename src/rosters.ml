open Asetmap
open Sexplib.Std
module Jid_map = Map.Make (Jid.Bare)

module Subscription = struct
  type t =
    | None
    | To
    | From
    | Both
    | Remove
  [@@deriving sexp]

  let to_string = function
    | None -> "none"
    | To -> "to"
    | From -> "from"
    | Both -> "both"
    | Remove -> "remove"
  ;;
end

module Presence = struct
  type t =
    | Online
    | Offline
  [@@deriving sexp]
end

module Item = struct
  type t =
    { handle : string
    ; subscription : Subscription.t [@default (None : Subscription.t)]
    ; ask : bool [@default false]
    ; groups : string list }
  [@@deriving sexp, make]

  let subscription t = t.subscription
  let to_tuple {handle; subscription; ask; groups} = handle, subscription, ask, groups
end

module Roster = struct
  type t = Item.t Jid_map.t

  let empty = Jid_map.empty

  let t_of_sexp s =
    Sexplib.Conv.list_of_sexp
      (fun jid_item ->
        Sexplib.Conv.pair_of_sexp Jid.Bare.t_of_sexp Item.t_of_sexp jid_item )
      s
    |> Jid_map.of_list
  ;;

  let sexp_of_t t =
    Jid_map.to_list t
    |> Sexplib.Conv.sexp_of_list (fun jid_item ->
           Sexplib.Conv.sexp_of_pair Jid.Bare.sexp_of_t Item.sexp_of_t jid_item )
  ;;
end

module User = struct
  type t =
    { presence : Presence.t [@default Presence.Offline]
    ; roster : Roster.t [@default Roster.empty]
    ; free : bool [@default true] }
  [@@deriving sexp, make]

  let set_presence presence t = {t with presence}
  let set_free free t = {t with free}
  let get_free t = t.free
end

let t = ref Jid_map.empty
let mutex = Lwt_mutex.create ()
let with_mutex f = Lwt_mutex.with_lock mutex f

let lock_user user =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some user_t ->
        if User.get_free user_t
        then (
          t := Jid_map.add user (User.set_free false user_t) !t;
          Lwt.return_true )
        else Lwt.return_false
      | None ->
        let new_user = User.make ~free:false () in
        t := Jid_map.add user new_user !t;
        Lwt.return_true )
;;

let unlock_user user =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some user_t ->
        t := Jid_map.add user (User.set_free true user_t) !t;
        Lwt.return_unit
      | None -> Lwt.return_unit )
;;

let sexp_of_t t =
  Jid_map.to_list t
  |> Sexplib.Conv.sexp_of_list (fun jid_user ->
         Sexplib.Conv.sexp_of_pair Jid.Bare.sexp_of_t User.sexp_of_t jid_user )
;;

let to_string () =
  with_mutex (fun () -> sexp_of_t !t |> Sexplib.Sexp.to_string_hum |> Lwt.return)
;;

let set_presence user presence =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some user_t ->
        let new_user_t = User.set_presence presence user_t in
        t := Jid_map.add user new_user_t !t;
        Lwt.return_unit
      | None ->
        let new_user_t = User.make ~presence () in
        t := Jid_map.add user new_user_t !t;
        Lwt.return_unit )
;;

let get_presence user =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some {presence; _} -> Lwt.return presence
      | None -> Lwt.return Presence.Offline )
;;

let remove_item user contact =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some ({roster; _} as user_t) ->
        let new_roster = Jid_map.remove contact roster in
        let new_user_t = {user_t with roster = new_roster} in
        t := Jid_map.add user new_user_t !t;
        Lwt.return_unit
      | None -> Lwt.return_unit )
;;

let set_item
    ?(subscription : Subscription.t = None) ?(handle = "") ?(groups = []) user contact =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some ({roster; _} as user_t) ->
        let new_item =
          match Jid_map.find contact roster with
          | Some item -> {item with handle; groups}
          | None -> Item.make ~handle ~subscription ~groups ()
        in
        let new_roster = Jid_map.add contact new_item roster in
        let new_user_t = {user_t with roster = new_roster} in
        t := Jid_map.add user new_user_t !t;
        Lwt.return new_item
      | None ->
        let new_item = Item.make ~handle ~subscription ~groups () in
        let new_roster = Jid_map.add contact new_item Jid_map.empty in
        t := Jid_map.add user (User.make ~roster:new_roster ()) !t;
        Lwt.return new_item )
;;

let get_item user contact =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some {roster; _} ->
        (match Jid_map.find contact roster with
        | Some item -> Lwt.return_some item
        | None -> Lwt.return_none)
      | None -> Lwt.return_none )
;;

let get_items user =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some {roster; _} -> Jid_map.to_list roster |> Lwt.return
      | None -> Lwt.return_nil )
;;

let update_subscription update_fun user contact =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some ({roster; _} as user_t) ->
        (match Jid_map.find contact roster with
        | Some ({subscription; _} as item) ->
          let new_subscription = update_fun subscription in
          let new_item = {item with subscription = new_subscription} in
          let new_roster = Jid_map.add contact new_item roster in
          let new_user_t = {user_t with roster = new_roster} in
          t := Jid_map.add user new_user_t !t;
          Lwt.return_unit
        | None -> Lwt.return_unit)
      | None -> Lwt.return_unit )
;;

let downgrade_subscription_from user contact =
  update_subscription (function From -> None | Both -> To | sub -> sub) user contact
;;

let downgrade_subscription_to user contact =
  update_subscription (function To -> None | Both -> From | sub -> sub) user contact
;;

let upgrade_subscription_to user contact =
  update_subscription (function None -> To | From -> Both | sub -> sub) user contact
;;

let upgrade_subscription_from user contact =
  update_subscription (function None -> From | To -> Both | sub -> sub) user contact
;;

let get_item_field user contact get_field =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some {roster; _} ->
        (match Jid_map.find contact roster with
        | Some item -> Lwt.return_some (get_field item)
        | None -> Lwt.return_none)
      | None -> Lwt.return_none )
;;

let get_ask user contact = get_item_field user contact (fun item -> item.ask)

let get_subscription user contact =
  get_item_field user contact (fun item -> item.subscription)
;;

let get_contacts_with_subscription user subscription_match =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some {roster; _} ->
        Jid_map.to_list roster
        |> List.filter (fun (_, item) -> subscription_match @@ Item.subscription item)
        |> List.map (fun (jid, _) -> jid)
        |> Lwt.return
      | None -> Lwt.return_nil )
;;

let get_subscriptions user =
  get_contacts_with_subscription user (function To | Both -> true | _ -> false)
;;

let get_subscribers user =
  get_contacts_with_subscription user (function From | Both -> true | _ -> false)
;;

let update_ask user contact value =
  with_mutex (fun () ->
      match Jid_map.find user !t with
      | Some ({roster; _} as user_t) ->
        (match Jid_map.find contact roster with
        | Some item ->
          let new_item = {item with ask = value} in
          let new_roster = Jid_map.add contact new_item roster in
          let new_user_t = {user_t with roster = new_roster} in
          t := Jid_map.add user new_user_t !t;
          Lwt.return_unit
        | None -> Lwt.return_unit)
      | None -> Lwt.return_unit )
;;

let unset_ask user contact = update_ask user contact false
let set_ask user contact = update_ask user contact true

let clear () =
  with_mutex (fun () ->
      t := Jid_map.empty;
      Lwt.return_unit )
;;

let test_rosters actions =
  let test =
    let%lwt () = clear () in
    let%lwt _ = actions () in
    let%lwt s = to_string () in
    let%lwt () = Lwt_io.printl s in
    Lwt_io.flush_all ()
  in
  Lwt_main.run test
;;

let%expect_test "empty initially" =
  test_rosters (fun () -> Lwt.return_unit);
  [%expect {| () |}]
;;

let%expect_test "add one jid" =
  test_rosters (fun () ->
      set_item
        ~handle:"my romeo"
        ~groups:[]
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription None) (ask false) (groups ())))))
         (free true)))) |}]
;;

let%expect_test "add one jid with groups" =
  test_rosters (fun () ->
      set_item
        ~handle:"my romeo"
        ~groups:["Group1"; "Group2"]
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription None) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "add same jid multiple times" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      set_item
        ~handle:"my romeo"
        ~groups:["Group1"; "Group2"]
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription None) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "add same jid multiple times with different groups" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      set_item
        ~handle:"king1"
        ~groups:["Kings1"; "Rulers"; "Others"]
        (Jid.Bare.of_string "lord@im.example.com")
        (Jid.Bare.of_string "king@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription None) (ask false)
             (groups (Group1 Group2))))))
         (free true)))
       ((lord im.example.com)
        ((presence Offline)
         (roster
          (((king im.example.com)
            ((handle king1) (subscription None) (ask false)
             (groups (Kings1 Rulers Others))))))
         (free true)))) |}]
;;

let%expect_test "add two different jids to the roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      set_item
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "other@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((other im.example.com)
            ((handle "") (subscription None) (ask false) (groups ())))
           ((romeo im.example.com)
            ((handle "") (subscription None) (ask false) (groups ())))))
         (free true)))) |}]
;;

let%expect_test "removing an item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      remove_item
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
    (((juliet im.example.com) ((presence Offline) (roster ()) (free true)))) |}]
;;

let%expect_test "removing a non-existant item" =
  test_rosters (fun () ->
      remove_item
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect {| () |}]
;;

let%expect_test "downgrade subscription from when no items in roster" =
  test_rosters (fun () ->
      downgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect {| () |}]
;;

let%expect_test "downgrade subscription from when no valid item in roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "other@im.example.com")
      in
      downgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((other im.example.com)
            ((handle "") (subscription None) (ask false) (groups ())))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription from with None item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      downgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription None) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription from with To item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:To
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      downgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription To) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription from with From item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:From
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      downgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription None) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription from with Both item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:Both
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      downgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription To) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription to when no items in roster" =
  test_rosters (fun () ->
      downgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect {| () |}]
;;

let%expect_test "downgrade subscription to when no valid item in roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "other@im.example.com")
      in
      downgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((other im.example.com)
            ((handle "") (subscription None) (ask false) (groups ())))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription to with None item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      downgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription None) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription to with To item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:To
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      downgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription None) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription to with From item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:From
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      downgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription From) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "downgrade subscription to with Both item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:Both
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      downgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription From) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription to when no items in roster" =
  test_rosters (fun () ->
      upgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect {| () |}]
;;

let%expect_test "upgrade subscription to when no valid item in roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "other@im.example.com")
      in
      upgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((other im.example.com)
            ((handle "") (subscription None) (ask false) (groups ())))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription to with None item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      upgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription To) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription to with To item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:To
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      upgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription To) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription to with From item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:From
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      upgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription Both) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription to with Both item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:Both
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      upgrade_subscription_to
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription Both) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription from when no items in roster" =
  test_rosters (fun () ->
      upgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect {| () |}]
;;

let%expect_test "upgrade subscription from when no valid item in roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "other@im.example.com")
      in
      upgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((other im.example.com)
            ((handle "") (subscription None) (ask false) (groups ())))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription from with None item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      upgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription From) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription from with To item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:To
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      upgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription Both) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription from with From item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:From
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      upgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription From) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "upgrade subscription from with Both item" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          ~subscription:Both
          ~handle:"my romeo"
          ~groups:["Group1"; "Group2"]
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      upgrade_subscription_from
        (Jid.Bare.of_string "juliet@im.example.com")
        (Jid.Bare.of_string "romeo@im.example.com") );
  [%expect
    {|
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "my romeo") (subscription Both) (ask false)
             (groups (Group1 Group2))))))
         (free true)))) |}]
;;

let%expect_test "get ask on empty roster" =
  test_rosters (fun () ->
      let%lwt ask =
        get_ask
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      Lwt_io.printl @@ Utils.option_to_string string_of_bool ask );
  [%expect {|
    None
    () |}]
;;

let%expect_test "get ask with user in roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      let%lwt ask =
        get_ask
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      Lwt_io.printl @@ Utils.option_to_string string_of_bool ask );
  [%expect
    {|
      Some: false
      (((juliet im.example.com)
        ((presence Offline)
         (roster
          (((romeo im.example.com)
            ((handle "") (subscription None) (ask false) (groups ())))))
         (free true)))) |}]
;;

let%expect_test "get ask with user not in roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      let%lwt ask =
        get_ask
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "other@im.example.com")
      in
      Lwt_io.printl @@ Utils.option_to_string string_of_bool ask );
  [%expect
    {|
    None
    (((juliet im.example.com)
      ((presence Offline)
       (roster
        (((romeo im.example.com)
          ((handle "") (subscription None) (ask false) (groups ())))))
       (free true)))) |}]
;;

let%expect_test "get subscription with empty roster" =
  test_rosters (fun () ->
      let%lwt sub =
        get_subscription
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      Lwt_io.printl @@ Utils.option_to_string Subscription.to_string sub );
  [%expect {|
    None
    () |}]
;;

let%expect_test "get subscription with user in roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      let%lwt sub =
        get_subscription
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      Lwt_io.printl @@ Utils.option_to_string Subscription.to_string sub );
  [%expect
    {|
    Some: none
    (((juliet im.example.com)
      ((presence Offline)
       (roster
        (((romeo im.example.com)
          ((handle "") (subscription None) (ask false) (groups ())))))
       (free true)))) |}]
;;

let%expect_test "get subscription with user not in roster" =
  test_rosters (fun () ->
      let%lwt _ =
        set_item
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "romeo@im.example.com")
      in
      let%lwt sub =
        get_subscription
          (Jid.Bare.of_string "juliet@im.example.com")
          (Jid.Bare.of_string "other@im.example.com")
      in
      Lwt_io.printl @@ Utils.option_to_string Subscription.to_string sub );
  [%expect
    {|
    None
    (((juliet im.example.com)
      ((presence Offline)
       (roster
        (((romeo im.example.com)
          ((handle "") (subscription None) (ask false) (groups ())))))
       (free true)))) |}]
;;
