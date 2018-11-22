(* The state representing the current status of the connection *)
open Stanza
open Events

type state =
  | IDLE
  | NEGOTIATING
  | CLOSED

type t = {state : state}

let state_to_string = function
  | IDLE -> "idle"
  | NEGOTIATING -> "negotiating"
  | CLOSED -> "closed"
;;

let create () = {state = IDLE}
let to_string t = "{state: " ^ state_to_string t.state ^ "}"
let closed t = t.state = CLOSED

let handle_idle _t = function
  | STREAM_HEADER s ->
    (* new incoming connection *)
    (match s with
    | Stanza ((_name, attrs), _children) ->
      (* construct reply *)
      let from = get_value (get_attribute_by_name_exn attrs "to") in
      let dest = get_value (get_attribute_by_name_exn attrs "from") in
      let header = stream_header from dest in
      (* check the version attribute *)
      (match get_attribute_by_name attrs "version" with
      | Some attr ->
        if float_of_string (get_value attr) >= 1.0
        then
          let features = features in
          ( {state = NEGOTIATING}
          , [ Actions.SET_JID (Jid.of_string dest)
            ; Actions.REPLY_STANZA (false, header)
            ; Actions.REPLY_STANZA (true, features) ] )
        else assert false
      | None -> assert false)
    | Text _t -> {state = CLOSED}, [Actions.ERROR "Invalid stream header"])
  | RESOURCE_BIND_SERVER_GEN _ -> assert false
  | RESOURCE_BIND_CLIENT_GEN _ -> assert false
  | CLOSE -> {state = CLOSED}, [Actions.ERROR "No stream"]
  | ERROR e -> {state = CLOSED}, [Actions.ERROR e]
;;

let handle_negotiating _t = function
  | STREAM_HEADER _s -> {state = CLOSED}, [Actions.ERROR "Not expecting stream header"]
  | RESOURCE_BIND_SERVER_GEN id ->
    ( {state = NEGOTIATING}
    , [ Actions.REPLY_STANZA
          ( true
          , Stanza.create_iq_bind
              id
              ~children:
                [ Stanza.create
                    (("", "jid"), [])
                    ~children:[Stanza.Text [Jid.create_resource ()]] ] ) ] )
  | RESOURCE_BIND_CLIENT_GEN (id, res) ->
    {state = NEGOTIATING}, [Actions.SET_JID_RESOURCE (id, res)]
  | CLOSE ->
    (* the stream can close during negotiation so close our direction too *)
    {state = CLOSED}, [Actions.CLOSE]
  | ERROR e -> {state = CLOSED}, [Actions.ERROR e]
;;

let handle_closed _t = function
  | STREAM_HEADER _s -> {state = CLOSED}, [Actions.ERROR "Not expecting stream header"]
  | RESOURCE_BIND_SERVER_GEN _ -> assert false
  | RESOURCE_BIND_CLIENT_GEN _ -> assert false
  | Events.CLOSE ->
    (* shouldn't receive another close after being closed *)
    {state = CLOSED}, [Actions.ERROR "Not expecting a close"]
  | Events.ERROR e -> {state = CLOSED}, [Actions.ERROR e]
;;

let handle t event =
  match t.state with
  | IDLE -> handle_idle t event
  | NEGOTIATING -> handle_negotiating t event
  | CLOSED -> handle_closed t event
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
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    set_jid: juliet@im.example.com
    reply_stanza:
    <stream:stream
      from='im.example.com'
      id='redacted_for_testing'
      to='juliet@im.example.com'
      version='1.0'
      xml:lang='en'
      xmlns='jabber:client'
      xmlns:stream='http://etherx.jabber.org/streams'>
    reply_stanza:
    <stream:features>
      <bind
      xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
    </stream:features> |}]
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
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    set_jid: juliet@im.example.com
    reply_stanza:
    <stream:stream
      from='im.example.com'
      id='redacted_for_testing'
      to='juliet@im.example.com'
      version='1.0'
      xml:lang='en'
      xmlns='jabber:client'
      xmlns:stream='http://etherx.jabber.org/streams'>
    reply_stanza:
    <stream:features>
      <bind
      xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
    </stream:features> |}]
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
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    set_jid: juliet@im.example.com
    reply_stanza:
    <stream:stream
      from='im.example.com'
      id='redacted_for_testing'
      to='juliet@im.example.com'
      version='1.0'
      xml:lang='en'
      xmlns='jabber:client'
      xmlns:stream='http://etherx.jabber.org/streams'>
    reply_stanza:
    <stream:features>
      <bind
      xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
    </stream:features> |}];
  let fsm, actions = handle fsm Events.CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| close |}]
;;

let%expect_test "bind resource" =
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
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    set_jid: juliet@im.example.com
    reply_stanza:
    <stream:stream
      from='im.example.com'
      id='redacted_for_testing'
      to='juliet@im.example.com'
      version='1.0'
      xml:lang='en'
      xmlns='jabber:client'
      xmlns:stream='http://etherx.jabber.org/streams'>
    reply_stanza:
    <stream:features>
      <bind
      xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
    </stream:features> |}];
  let fsm, actions = handle fsm (Events.RESOURCE_BIND_SERVER_GEN "id") in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (fun s -> print_endline s) strings;
  [%expect
    {|
    reply_stanza:
    <iq
      id='redacted_for_testing'
      type='result'>
      <bind
      xmlns='urn:ietf:params:xml:ns:xmpp-bind'>
        <jid>
          not-implemented
        </jid>
      </bind>
    </iq> |}];
  let fsm, actions = handle fsm Events.CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| close |}]
;;

let%expect_test "bind resource client" =
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
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    set_jid: juliet@im.example.com
    reply_stanza:
    <stream:stream
      from='im.example.com'
      id='redacted_for_testing'
      to='juliet@im.example.com'
      version='1.0'
      xml:lang='en'
      xmlns='jabber:client'
      xmlns:stream='http://etherx.jabber.org/streams'>
    reply_stanza:
    <stream:features>
      <bind
      xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
    </stream:features> |}];
  let fsm, actions = handle fsm (Events.RESOURCE_BIND_CLIENT_GEN ("id", "client-res")) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (fun s -> print_endline s) strings;
  [%expect {|
    set_jid_resource: id=id res=client-res |}];
  let fsm, actions = handle fsm Events.CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| close |}]
;;
