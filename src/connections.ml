open Asetmap
module Jid_map = Map.Make (Jid.Full)

let mutex = Lwt_mutex.create ()
let with_mutex f = Lwt_mutex.with_lock mutex f
let t = ref Jid_map.empty

let add jid (f : Actions.t option -> unit) =
  with_mutex (fun () ->
      t := Jid_map.add jid f !t;
      Lwt.return_unit )
;;

let find jid = with_mutex (fun () -> Jid_map.find jid !t |> Lwt.return)

let find_all bare_jid =
  with_mutex (fun () ->
      Jid_map.filter (fun fjid _ -> bare_jid = Jid.Full.to_bare fjid) !t
      |> Jid_map.to_list
      |> Lwt.return )
;;

let remove jid =
  with_mutex (fun () ->
      t := Jid_map.remove jid !t;
      Lwt.return_unit )
;;

let clear () =
  with_mutex (fun () ->
      t := Jid_map.empty;
      Lwt.return_unit )
;;

let to_string () =
  with_mutex (fun () ->
      Jid_map.to_list !t
      |> Sexplib.Conv.sexp_of_list (fun jid_push ->
             Sexplib.Conv.sexp_of_pair
               Jid.Full.sexp_of_t
               Sexplib.Conv.sexp_of_fun
               jid_push )
      |> Sexplib.Sexp.to_string_hum
      |> Lwt.return )
;;

let test_connections actions =
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
  test_connections (fun () -> Lwt.return_unit);
  [%expect {| () |}]
;;

let%expect_test "add one connection" =
  ( test_connections
  @@ fun () -> add (Jid.Full.of_string "juliet@im.example.com/balcony") (fun _ -> ()) );
  [%expect {| ((((juliet im.example.com) balcony) <fun>)) |}]
;;

let%expect_test "add two connection" =
  ( test_connections
  @@ fun () ->
  let%lwt () = add (Jid.Full.of_string "juliet@im.example.com/balcony") (fun _ -> ()) in
  add (Jid.Full.of_string "romeo@home.elsewhere.com/ground") (fun _ -> ()) );
  [%expect
    {|
      ((((juliet im.example.com) balcony) <fun>)
       (((romeo home.elsewhere.com) ground) <fun>)) |}]
;;

let%expect_test "find all matches bare jid" =
  ( test_connections
  @@ fun () ->
  let%lwt () = add (Jid.Full.of_string "juliet@im.example.com/balcony") (fun _ -> ()) in
  let%lwt connected_resources = find_all (Jid.Bare.of_string "juliet@im.example.com") in
  Lwt_list.iter_s
    (fun (target_jid, _) -> Lwt_io.printl (Jid.Full.to_string target_jid))
    connected_resources );
  [%expect
    {|
    juliet@im.example.com/balcony
    ((((juliet im.example.com) balcony) <fun>)) |}]
;;
