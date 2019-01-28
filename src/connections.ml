open Asetmap
module Jid_map = Map.Make (Jid.Full)

let t = ref Jid_map.empty
let add jid (f : Actions.t option -> unit) = t := Jid_map.add jid f !t
let find jid = Jid_map.find jid !t

let find_all = function
  | Jid.Full_JID jid ->
    (match find jid with Some actions_push -> [jid, actions_push] | None -> [])
  | Jid.Bare_JID bare_jid ->
    Jid_map.filter (fun fjid _ -> bare_jid = Jid.Full.to_bare fjid) !t |> Jid_map.to_list
  | Domain _ -> []
;;

let remove jid = t := Jid_map.remove jid !t

let to_string () =
  Jid_map.to_list !t
  |> Sexplib.Conv.sexp_of_list (fun jid_push ->
         Sexplib.Conv.sexp_of_pair Jid.Full.sexp_of_t Sexplib.Conv.sexp_of_fun jid_push
     )
  |> Sexplib.Sexp.to_string_hum
;;

let clear () = t := Jid_map.empty

let%expect_test "empty initially" =
  clear ();
  print_endline (to_string ());
  [%expect {| () |}]
;;

let%expect_test "add one connection" =
  clear ();
  add (Jid.Full.of_string "juliet@im.example.com/balcony") (fun _ -> ());
  print_endline (to_string ());
  [%expect {| ((((juliet im.example.com) balcony) <fun>)) |}]
;;

let%expect_test "add two connection" =
  clear ();
  add (Jid.Full.of_string "juliet@im.example.com/balcony") (fun _ -> ());
  add (Jid.Full.of_string "romeo@home.elsewhere.com/ground") (fun _ -> ());
  print_endline (to_string ());
  [%expect
    {|
      ((((juliet im.example.com) balcony) <fun>)
       (((romeo home.elsewhere.com) ground) <fun>)) |}]
;;

let%expect_test "find all matches bare jid" =
  clear ();
  add (Jid.Full.of_string "juliet@im.example.com/balcony") (fun _ -> ());
  print_endline (to_string ());
  [%expect {| ((((juliet im.example.com) balcony) <fun>)) |}];
  find_all (Jid.of_string "juliet@im.example.com")
  |> List.iter (fun (target_jid, _actions_push) ->
         print_endline (Jid.Full.to_string target_jid) );
  [%expect {| juliet@im.example.com/balcony |}]
;;
