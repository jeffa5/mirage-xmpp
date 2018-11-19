(* The state representing the current status of the connection *)
open Stanza

exception MatchingError

type state =
  | IDLE
  | NEGOTIATING
  | CLOSING

type t = {state : state}

let state_to_string = function IDLE -> "idle" | NEGOTIATING -> "negotiating" | CLOSING -> "closing"
let create () = {state = IDLE}
let to_string t = "{state: " ^ state_to_string t.state ^ "}"

let update t event =
  let open Events in
  match event with
  | STANZA s ->
    (match t.state with
     | IDLE ->
       (match s with
        | Stanza (((_, "stream"), attrs), []) ->
          (* stream header stanza received, send own stream header *)
          let from = get_value (get_attribute_by_name_exn attrs "to") in
          let dest = get_value (get_attribute_by_name_exn attrs "from") in
          let header = stream_header from dest in
          (* If the version attribute is 1.0 then we need to send a features stanza to the client *)
          (match get_attribute_by_name attrs "version" with
           | Some attr ->
             if float_of_string (get_value attr) >= 1.0
             then
               (* Only allow headers with version >= 1.0 *)
               let features = features () in
               ( {state = NEGOTIATING}
               , [ Actions.REPLY_STANZA (false, header)
                 ; Actions.REPLY_STANZA (true, features) ] )
             else raise MatchingError
           | None -> raise MatchingError)
        | _ -> raise MatchingError)
     | NEGOTIATING -> raise MatchingError
     | CLOSING -> { state = CLOSING}, [])
  | CLOSE -> {state = CLOSING}, [Actions.CLOSE]
;;

let%expect_test "create" =
  let fsm = create () in
  print_endline (to_string fsm);
  [%expect {| {state: idle} |}]
;;

let%expect_test "idle to negotiating" =
  let fsm = create () in
  let stanza =
    Stanza.create
      ( ("stream", "stream")
      , [ ("", "from"), "juliet@im.example.com"
        ; ("", "to"), "im.example.com"
        ; ("", "version"), "1.0"
        ; ("xml", "lang"), "en"
        ; ("", "xmlns"), "jabber:client"
        ; ("xmlns", "stream"), "http://etherx.jabber.org/streams" ] )
  in
  let fsm, actions = update fsm (Events.STANZA stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
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
      xmlns:stream='http://etherx.jabber.org/streams'>
    reply_stanza:
    <stream:features /> |}]
;;

let%expect_test "idle to negotiating with > 1.0" =
  let fsm = create () in
  let stanza =
    Stanza.create
      ( ("stream", "stream")
      , [ ("", "from"), "juliet@im.example.com"
        ; ("", "to"), "im.example.com"
        ; ("", "version"), "2.0"
        ; ("xml", "lang"), "en"
        ; ("", "xmlns"), "jabber:client"
        ; ("xmlns", "stream"), "http://etherx.jabber.org/streams" ] )
  in
  let fsm, actions = update fsm (Events.STANZA stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
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
      xmlns:stream='http://etherx.jabber.org/streams'>
    reply_stanza:
    <stream:features /> |}]
;;

let%expect_test "negotiating to closing" =
  let fsm = create () in
  let stanza =
    Stanza.create
      ( ("stream", "stream")
      , [ ("", "from"), "juliet@im.example.com"
        ; ("", "to"), "im.example.com"
        ; ("", "version"), "1.0"
        ; ("xml", "lang"), "en"
        ; ("", "xmlns"), "jabber:client"
        ; ("xmlns", "stream"), "http://etherx.jabber.org/streams" ] )
  in
  let fsm, actions = update fsm (Events.STANZA stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    reply_stanza:
    <stream:stream
      from='im.example.com'
      id=''
      to='juliet@im.example.com'
      version='1.0'
      xml:lang='en'
      xmlns='jabber:client'
      xmlns:stream='http://etherx.jabber.org/streams'>
    reply_stanza:
    <stream:features /> |}];
  let fsm, actions = update fsm Events.CLOSE in
  print_endline (to_string fsm);
  [%expect{| {state: closing} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| close |}]
;;
