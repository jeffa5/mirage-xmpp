open Asetmap
module Jid_map = Map.Make (Jid)

let t = ref Jid_map.empty
let add jid f = t := Jid_map.add jid f !t
let find jid = Jid_map.find jid !t

let find_all = function
  | Jid.Full_JID (_bare_jid, _resource) as jid ->
    (match find jid with Some actions_push -> [jid, actions_push] | None -> [])
  | Jid.Bare_JID bare_jid ->
    Jid_map.filter
      (fun jid _actions_fun ->
        match jid with
        | Full_JID (bjid, _) -> bare_jid = bjid
        | Bare_JID bjid -> bare_jid = bjid
        | _ -> false )
      !t
    |> Jid_map.to_list
  | _ -> []
;;

let remove jid = t := Jid_map.remove jid !t

let to_string () =
  Sexplib.Sexp.to_string_hum
  @@ Sexplib.Conv.sexp_of_list (fun (jid, _) -> Jid.sexp_of_t jid)
  @@ Jid_map.to_list !t
;;

let clear () = t := Jid_map.empty

let%expect_test "empty initially" =
  clear ();
  print_endline (to_string ());
  [%expect {| () |}]
;;

let%expect_test "add one connection" =
  clear ();
  add (Jid.of_string "juliet@im.example.com") (fun _ -> ());
  print_endline (to_string ());
  [%expect {| ((Bare_JID (juliet im.example.com))) |}]
;;

let%expect_test "add two connection" =
  clear ();
  add (Jid.of_string "juliet@im.example.com") (fun _ -> ());
  add (Jid.of_string "romeo@home.elsewhere.com") (fun _ -> ());
  print_endline (to_string ());
  [%expect
    {|
      ((Bare_JID (romeo home.elsewhere.com)) (Bare_JID (juliet im.example.com))) |}]
;;

let%expect_test "find all matches bare jid" =
  clear ();
  add (Jid.of_string "juliet@im.example.com/balcony") (fun _ -> ());
  print_endline (to_string ());
  [%expect {| ((Full_JID ((juliet im.example.com) balcony))) |}];
  find_all (Jid.of_string "juliet@im.example.com")
  |> List.iter (fun (target_jid, _actions_push) ->
         print_endline (Jid.to_string target_jid) );
  [%expect {| juliet@im.example.com/balcony |}]
;;
