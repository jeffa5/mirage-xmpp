(* The state representing the current status of the connection *)
open Stanza

type state =
  | IDLE
  | OPENING

type t = {state : state}

let state_to_string = function IDLE -> "idle" | OPENING -> "opening"
let create () = {state = IDLE}
let to_string t = "{state: " ^ state_to_string t.state ^ "}"

let update t event =
  let open Events in
  match event with STANZA s ->
    (match t with
    | {state = IDLE} ->
      (match s with
      | Stanza (((_, "stream"), attrs), []) ->
        (* stream header stanza received, send own stream header *)
        let from = get_value (get_attribute_by_name attrs "to") in
        let dest = get_value (get_attribute_by_name attrs "from") in
        let header = stream_header from dest in
        {state = OPENING}, [Actions.REPLY_STANZA (false, header)]
      | _ -> assert false)
    | _ -> assert false)
;;

let%expect_test "create" =
  let fsm = create () in
  print_endline (to_string fsm);
  [%expect {| {state: idle} |}]
;;

let%expect_test "idle to opening" =
  let fsm = create () in
  let stanza =
    Stanza.create
      ( ("stream", "stream")
      , [("", "from"), "juliet@im.example.com"; ("", "to"), "im.example.com"] )
  in
  let fsm, actions = update fsm (Events.STANZA stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: opening} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    reply_stanza:
    <stream:stream
      from='im.example.com'
      id=''
      to='juliet@im.example.com'
      version='1.0'
      xml:lang='en'
      xmlns='jabber:client'
      xmlns:stream='http://etherx.jabber.org/streams'> |}]
;;
