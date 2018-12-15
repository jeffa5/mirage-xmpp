(* The state representing the current status of the connection *)
open Stanza
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
  | STREAM_HEADER (_name, attrs) ->
    (* new incoming connection *)
    (* construct reply *)
    let from = get_value (get_attribute_by_name_exn attrs "to") in
    let dest = get_value (get_attribute_by_name_exn attrs "from") in
    let header = create_stream_header from dest in
    (* check the version attribute *)
    (match get_attribute_by_name attrs "version" with
    | Some attr ->
      if float_of_string (get_value attr) >= 1.0
      then
        ( {state = NEGOTIATING}
        , [ Actions.SET_JID (Jid.of_string dest)
          ; Actions.SEND_STREAM_HEADER header
          ; Actions.SEND_STREAM_FEATURES Stream.features ] )
      else assert false
    | None -> assert false)
  | RESOURCE_BIND_SERVER_GEN _ -> assert false
  | RESOURCE_BIND_CLIENT_GEN _ -> assert false
  | STREAM_CLOSE -> {state = CLOSED}, [Actions.ERROR "No stream"]
  | ERROR e -> {state = CLOSED}, [Actions.ERROR e]
  | ROSTER_GET (_from, _id) -> {state = CLOSED}, [Actions.ERROR "No stream"]
;;

let handle_negotiating _t = function
  | STREAM_HEADER _s -> {state = CLOSED}, [Actions.ERROR "Not expecting stream header"]
  | RESOURCE_BIND_SERVER_GEN id ->
    ( {state = NEGOTIATING}
    , [ Actions.REPLY_STANZA
          (Stanza.create_iq_bind
             id
             ~children:
               [ Xml.create
                   (("", "jid"), [])
                   ~children:[Xml.Text (Jid.create_resource ())] ]) ] )
  | RESOURCE_BIND_CLIENT_GEN (id, res) ->
    {state = NEGOTIATING}, [Actions.SET_JID_RESOURCE (id, res)]
  | STREAM_CLOSE ->
    (* the stream can close during negotiation so close our direction too *)
    {state = CLOSED}, [Actions.CLOSE]
  | ERROR e -> {state = CLOSED}, [Actions.ERROR e]
  | ROSTER_GET (from, id) -> {state = CONNECTED}, [Actions.GET_ROSTER (from, id)]
;;

let handle_connected _t = function
  | STREAM_HEADER _ -> {state = CLOSED}, [Actions.ERROR "Not expecting stream header"]
  | RESOURCE_BIND_SERVER_GEN _ -> assert false
  | RESOURCE_BIND_CLIENT_GEN _ -> assert false
  | STREAM_CLOSE -> {state = CLOSED}, [Actions.CLOSE]
  | ERROR e -> {state = CLOSED}, [Actions.ERROR e]
  | ROSTER_GET (from, id) -> {state = CONNECTED}, [Actions.GET_ROSTER (from, id)]
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
  let header =
    Stanza.create_stream_header
      ~version:"1.0"
      ~lang:"en"
      ~xmlns:"jabber:client"
      "juliet@im.example.com"
      "im.example.com"
  in
  let fsm, actions = handle fsm (Events.STREAM_HEADER header) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: <stream:stream from='im.example.com' id='<redacted_for_testing>' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'/>
    SEND_STREAM_FEATURES: <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features> |}]
;;

let%expect_test "idle to negotiating with > 1.0" =
  let fsm = create () in
  let stanza =
    Stanza.create_stream_header
      ~version:"2.0"
      ~lang:"en"
      ~xmlns:"jabber:client"
      "juliet@im.example.com"
      "im.example.com"
  in
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: <stream:stream from='im.example.com' id='<redacted_for_testing>' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'/>
    SEND_STREAM_FEATURES: <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features> |}]
;;

let%expect_test "negotiating to closing" =
  let fsm = create () in
  let stanza =
    Stanza.create_stream_header
      ~version:"1.0"
      ~lang:"en"
      ~xmlns:"jabber:client"
      "juliet@im.example.com"
      "im.example.com"
  in
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: <stream:stream from='im.example.com' id='<redacted_for_testing>' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'/>
    SEND_STREAM_FEATURES: <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features> |}];
  let fsm, actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| CLOSE |}]
;;

let%expect_test "bind resource" =
  let fsm = create () in
  let stanza =
    Stanza.create_stream_header
      ~version:"1.0"
      ~lang:"en"
      ~xmlns:"jabber:client"
      "juliet@im.example.com"
      "im.example.com"
  in
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: <stream:stream from='im.example.com' id='<redacted_for_testing>' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'/>
    SEND_STREAM_FEATURES: <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features> |}];
  let fsm, actions = handle fsm (Events.RESOURCE_BIND_SERVER_GEN "id") in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (fun s -> print_endline s) strings;
  [%expect
    {|
    REPLY_STANZA: <iq id='<redacted_for_testing>' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>not-implemented</jid></bind></iq> |}];
  let fsm, actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {| CLOSE |}]
;;

let%expect_test "bind resource client" =
  let fsm = create () in
  let stanza =
    Stanza.create_stream_header
      ~version:"1.0"
      ~lang:"en"
      ~xmlns:"jabber:client"
      "juliet@im.example.com"
      "im.example.com"
  in
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: <stream:stream from='im.example.com' id='<redacted_for_testing>' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'/>
    SEND_STREAM_FEATURES: <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features> |}];
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
  let stanza =
    Stanza.create_stream_header
      ~version:"1.0"
      ~lang:"en"
      ~xmlns:"jabber:client"
      "juliet@im.example.com"
      "im.example.com"
  in
  let fsm, actions = handle fsm (Events.STREAM_HEADER stanza) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect
    {|
    SET_JID: juliet@im.example.com
    SEND_STREAM_HEADER: <stream:stream from='im.example.com' id='<redacted_for_testing>' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'/>
    SEND_STREAM_FEATURES: <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features> |}];
  let fsm, actions = handle fsm (Events.RESOURCE_BIND_CLIENT_GEN ("id", "client-res")) in
  print_endline (to_string fsm);
  [%expect {| {state: negotiating} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect {| SET_JID_RESOURCE: id=id res=client-res |}];
  let fsm, actions = handle fsm (Events.ROSTER_GET ("from", "id")) in
  print_endline (to_string fsm);
  [%expect {| {state: connected} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect {| GET_ROSTER: from=from id=id |}];
  let fsm, actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: closed} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect {| CLOSE |}]
;;
