(* The state representing the current status of the connection *)
open Events

type state =
  | IDLE
  | NEGOTIATING
  | CONNECTED
  | CLOSED

type t = {state : state}

let state_to_string = function
  | IDLE -> "idle"
  | NEGOTIATING -> "negotiating"
  | CONNECTED -> "connected"
  | CLOSED -> "closed"
;;

let create () = {state = IDLE}
let to_string t = "{state: " ^ state_to_string t.state ^ "}"
let closed t = t.state = CLOSED

let handle_idle _t = function
  | STREAM_HEADER {from; ato; version} ->
    (* new incoming connection *)
    (* construct reply *)
    (* check the version attribute *)
    if float_of_string version >= 1.0
    then
      ( {state = NEGOTIATING}
      , [ Actions.SET_JID from
        ; Actions.SEND_STREAM_HEADER {from; ato}
        ; Actions.SEND_STREAM_FEATURES ] )
    else assert false
  | RESOURCE_BIND_SERVER_GEN _ -> assert false
  | RESOURCE_BIND_CLIENT_GEN _ -> assert false
  | STREAM_CLOSE -> {state = CLOSED}, [Actions.ERROR "No stream"]
  | ERROR e -> {state = CLOSED}, [Actions.ERROR e]
  | ROSTER_GET (_from, _id) -> {state = CLOSED}, [Actions.ERROR "No stream"]
  | ROSTER_SET (_id, _from, _target, _handle, _subscribed, _groups) ->
    {state = CLOSED}, [Actions.ERROR "No stream"]
;;

let handle_negotiating _t = function
  | STREAM_HEADER _s -> {state = CLOSED}, [Actions.ERROR "Not expecting stream header"]
  | RESOURCE_BIND_SERVER_GEN id ->
    {state = NEGOTIATING}, [Actions.SERVER_GEN_RESOURCE_IDENTIFIER id]
  | RESOURCE_BIND_CLIENT_GEN (id, res) ->
    {state = NEGOTIATING}, [Actions.SET_JID_RESOURCE (id, res)]
  | STREAM_CLOSE ->
    (* the stream can close during negotiation so close our direction too *)
    {state = CLOSED}, [Actions.CLOSE]
  | ERROR e -> {state = CLOSED}, [Actions.ERROR e]
  | ROSTER_GET (from, id) ->
    {state = CONNECTED}, [Actions.ADD_TO_CONNECTIONS; Actions.GET_ROSTER (from, id)]
  | ROSTER_SET (id, from, target, handle, subscribed, groups) ->
    ( {state = CONNECTED}
    , [ Actions.ADD_TO_CONNECTIONS
      ; Actions.SET_ROSTER (id, from, target, handle, subscribed, groups)
      ; Actions.PUSH_ROSTER (Jid.to_bare from, target) ] )
;;

let handle_connected _t = function
  | STREAM_HEADER _ ->
    ( {state = CLOSED}
    , [Actions.REMOVE_FROM_CONNECTIONS; Actions.ERROR "Not expecting stream header"] )
  | RESOURCE_BIND_SERVER_GEN _ -> assert false
  | RESOURCE_BIND_CLIENT_GEN _ -> assert false
  | STREAM_CLOSE -> {state = CLOSED}, [Actions.REMOVE_FROM_CONNECTIONS; Actions.CLOSE]
  | ERROR e -> {state = CLOSED}, [Actions.REMOVE_FROM_CONNECTIONS; Actions.ERROR e]
  | ROSTER_GET (from, id) -> {state = CONNECTED}, [Actions.GET_ROSTER (from, id)]
  | ROSTER_SET (id, from, target, handle, subscribed, groups) ->
    ( {state = CONNECTED}
    , [ Actions.SET_ROSTER (id, from, target, handle, subscribed, groups)
      ; Actions.PUSH_ROSTER (Jid.to_bare from, target) ] )
;;

let handle_closed _t = function
  | STREAM_HEADER _s -> {state = CLOSED}, [Actions.ERROR "Not expecting stream header"]
  | RESOURCE_BIND_SERVER_GEN _ -> assert false
  | RESOURCE_BIND_CLIENT_GEN _ -> assert false
  | STREAM_CLOSE ->
    (* shouldn't receive another close after being closed *)
    {state = CLOSED}, [Actions.ERROR "Not expecting a close"]
  | ERROR e -> {state = CLOSED}, [Actions.ERROR e]
  | ROSTER_GET (_from, _id) -> {state = CLOSED}, [Actions.ERROR "already closed"]
  | ROSTER_SET (_id, _from, _target, _handle, _subscribed, _groups) ->
    {state = CLOSED}, [Actions.ERROR "already closed"]
;;

let handle t event =
  match t.state with
  | IDLE -> handle_idle t event
  | NEGOTIATING -> handle_negotiating t event
  | CONNECTED -> handle_connected t event
  | CLOSED -> handle_closed t event
;;

let%expect_test "create" =
  let fsm = create () in
  print_endline (to_string fsm);
  [%expect {| {state: idle} |}]
;;

let%expect_test "idle to negotiating" =
  let fsm = create () in
  let fsm, actions =
    handle
      fsm
      (Events.STREAM_HEADER
         { from = Jid.of_string "juliet@im.example.com"
         ; ato = Jid.of_string "im.example.com"
         ; version = "1.0" })
  in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: from=juliet@im.example.com to=im.example.com
    SEND_STREAM_FEATURES |}]
;;

let%expect_test "idle to negotiating with > 1.0" =
  let fsm = create () in
  let fsm, actions =
    handle
      fsm
      (Events.STREAM_HEADER
         { from = Jid.of_string "juliet@im.example.com"
         ; ato = Jid.of_string "im.example.com"
         ; version = "2.0" })
  in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: from=juliet@im.example.com to=im.example.com
    SEND_STREAM_FEATURES |}]
;;

let%expect_test "negotiating to closing" =
  let fsm = create () in
  let fsm, actions =
    handle
      fsm
      (Events.STREAM_HEADER
         { from = Jid.of_string "juliet@im.example.com"
         ; ato = Jid.of_string "im.example.com"
         ; version = "1.0" })
  in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: from=juliet@im.example.com to=im.example.com
    SEND_STREAM_FEATURES |}];
  let fsm, actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| CLOSE |}]
;;

let%expect_test "bind resource" =
  let fsm = create () in
  let fsm, actions =
    handle
      fsm
      (Events.STREAM_HEADER
         { from = Jid.of_string "juliet@im.example.com"
         ; ato = Jid.of_string "im.example.com"
         ; version = "1.0" })
  in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: from=juliet@im.example.com to=im.example.com
    SEND_STREAM_FEATURES |}];
  let fsm, actions = handle fsm (Events.RESOURCE_BIND_SERVER_GEN "id") in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (fun s -> print_endline s) strings;
  [%expect {|
    SERVER_GEN_RESOURCE_IDENTIFIER: id |}];
  let fsm, actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| CLOSE |}]
;;

let%expect_test "bind resource client" =
  let fsm = create () in
  let fsm, actions =
    handle
      fsm
      (Events.STREAM_HEADER
         { from = Jid.of_string "juliet@im.example.com"
         ; ato = Jid.of_string "im.example.com"
         ; version = "1.0" })
  in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: from=juliet@im.example.com to=im.example.com
    SEND_STREAM_FEATURES |}];
  let fsm, actions = handle fsm (Events.RESOURCE_BIND_CLIENT_GEN ("id", "client-res")) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (fun s -> print_endline s) strings;
  [%expect {|
    SET_JID_RESOURCE: id=id res=client-res |}];
  let fsm, actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| CLOSE |}]
;;

let%expect_test "roster get" =
  let fsm = create () in
  let fsm, actions =
    handle
      fsm
      (Events.STREAM_HEADER
         { from = Jid.of_string "juliet@im.example.com"
         ; ato = Jid.of_string "im.example.com"
         ; version = "1.0" })
  in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: from=juliet@im.example.com to=im.example.com
    SEND_STREAM_FEATURES |}];
  let fsm, actions = handle fsm (Events.RESOURCE_BIND_CLIENT_GEN ("id", "client-res")) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect {| SET_JID_RESOURCE: id=id res=client-res |}];
  let fsm, actions =
    handle fsm (Events.ROSTER_GET ("some_id", Jid.of_string "juliet@example.com"))
  in
  print_endline (to_string fsm);
  [%expect {| {state: connected} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect
    {|
    ADD_TO_CONNECTIONS
    GET_ROSTER: id=some_id from=juliet@example.com |}];
  let fsm, actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect {|
    REMOVE_FROM_CONNECTIONS
    CLOSE |}]
;;

let%expect_test "roster set" =
  let fsm = create () in
  let fsm, actions =
    handle
      fsm
      (Events.STREAM_HEADER
         { from = Jid.of_string "juliet@im.example.com"
         ; ato = Jid.of_string "im.example.com"
         ; version = "1.0" })
  in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: from=juliet@im.example.com to=im.example.com
    SEND_STREAM_FEATURES |}];
  let fsm, actions = handle fsm (Events.RESOURCE_BIND_CLIENT_GEN ("id", "client-res")) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect {| SET_JID_RESOURCE: id=id res=client-res |}];
  let fsm, actions =
    handle fsm (Events.ROSTER_GET ("some_id", Jid.of_string "juliet@example.com"))
  in
  print_endline (to_string fsm);
  [%expect {| {state: connected} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect
    {|
    ADD_TO_CONNECTIONS
    GET_ROSTER: id=some_id from=juliet@example.com |}];
  let fsm, actions =
    handle
      fsm
      (Events.ROSTER_SET
         ( "some_id"
         , Jid.of_string "juliet@example.com"
         , Jid.of_string "nurse@example.com"
         , "Nurse"
         , "none"
         , ["Servants"] ))
  in
  print_endline (to_string fsm);
  [%expect {| {state: connected} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect
    {|
    SET_ROSTER: id=some_id from=juliet@example.com target=nurse@example.com handle=Nurse subscribed=none groups=[Servants]
    PUSH_ROSTER: bare_jid=juliet@example.com updated_jid=nurse@example.com |}];
  let fsm, actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect {|
    REMOVE_FROM_CONNECTIONS
    CLOSE |}]
;;
