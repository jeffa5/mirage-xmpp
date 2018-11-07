(* The state representing the current status of the connection *)
type state =
  | IDLE
  | OPENING

type t = {state : state}

let state_to_string = function IDLE -> "idle" | OPENING -> "opening"
let create () = {state = IDLE}
let to_string {state} = state_to_string state

let update t event =
  let open Events in
  match event with
  | INITIAL s ->
    (match t with
    | {state = IDLE} -> {state = OPENING}, [Actions.SEND_STANZA s]
    | _ -> assert false)
  | STARTING -> t, []
  | CLOSING ->
    (match t with
    | {state = IDLE} -> {state = OPENING}, []
    | {state = OPENING} -> {state = IDLE}, [])
;;

let%expect_test "create" =
  let fsm = create () in
  print_endline (to_string fsm);
  [%expect {| idle |}]
;;

let%expect_test "idle to opening" =
  let fsm = create () in
  let stanza = Stanza.create (Stanza.Tag (("", "test"), [])) in
  let fsm, actions = update fsm (Events.INITIAL stanza) in
  print_endline (to_string fsm);
  [%expect {| opening |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s ") strings;
  [%expect {| send_xml |}]
;;
