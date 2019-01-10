(* The state representing the current status of the connection *)
open Events

type state =
  | IDLE
  | SASL_NEGOTIATION
  | NEGOTIATING
  | CONNECTED
  | CLOSED

type t = {state : state}

let state_to_string = function
  | IDLE -> "IDLE"
  | SASL_NEGOTIATION -> "SASL_NEGOTIATION"
  | NEGOTIATING -> "NEGOTIATING"
  | CONNECTED -> "CONNECTED"
  | CLOSED -> "CLOSED"
;;

let initial = {state = IDLE}
let to_string t = "{state: " ^ state_to_string t.state ^ "}"

let closed =
  ( {state = CLOSED}
  , [ Actions.UPDATE_PRESENCE Rosters.Offline
    ; Actions.REMOVE_FROM_CONNECTIONS
    ; Actions.CLOSE ]
  , [Actions.EXIT] )
;;

let closed_with_error e =
  ( {state = CLOSED}
  , [ Actions.UPDATE_PRESENCE Rosters.Offline
    ; Actions.REMOVE_FROM_CONNECTIONS
    ; Actions.ERROR e ]
  , [Actions.EXIT] )
;;

let handle_idle t = function
  | STREAM_HEADER {ato; version} ->
    (* new incoming connection *)
    (* check it was for us *)
    if ato <> Jid.Empty
    then
      if (* construct reply *)
         (* check the version attribute *)
         float_of_string version >= 1.0
      then
        ( {state = SASL_NEGOTIATION}
        , [Actions.SEND_STREAM_HEADER; Actions.SEND_STREAM_FEATURES_SASL]
        , [] )
      else closed_with_error "Must use version >= 1.0"
    else closed_with_error "Received empty to attribute"
  | SASL_AUTH _ -> closed_with_error "No stream"
  | RESOURCE_BIND_SERVER_GEN _ -> closed_with_error "No stream"
  | RESOURCE_BIND_CLIENT_GEN _ -> closed_with_error "No stream"
  | SESSION_START _id -> closed_with_error "No stream"
  | STREAM_CLOSE -> closed_with_error "No stream"
  | ERROR e -> closed_with_error e
  | ROSTER_GET _ -> closed_with_error "No stream"
  | ROSTER_SET _ -> closed_with_error "No stream"
  | ROSTER_REMOVE _ -> closed_with_error "No stream"
  | SUBSCRIPTION_REQUEST _ -> closed_with_error "No stream"
  | PRESENCE_UPDATE _ -> closed_with_error "No stream"
  | IQ_ERROR _ -> closed_with_error "No stream"
  | MESSAGE _ -> closed_with_error "No stream"
  | LOG_OUT -> closed_with_error "No stream"
  | NOOP -> t, [], []
  | SUBSCRIPTION_APPROVAL _ -> closed_with_error "No stream"
;;

let handle_sasl_negotiation t = function
  | STREAM_HEADER _ ->
    closed_with_error "Unexpected stream header during sasl negotiation"
  | SASL_AUTH {user; _} ->
    ( {state = NEGOTIATING}
    , [Actions.SET_JID user; Actions.SEND_SASL_SUCCESS]
    , [Actions.RESET_PARSER] )
  | RESOURCE_BIND_SERVER_GEN _ -> closed_with_error "Not finished SASL"
  | RESOURCE_BIND_CLIENT_GEN _ -> closed_with_error "Not finished SASL"
  | SESSION_START _id ->
    closed_with_error "Unexpected session start stanza during sasl negotiation"
  | STREAM_CLOSE -> closed_with_error "Unexpected stream close during sasl negotiation"
  | ERROR e -> closed_with_error e
  | ROSTER_GET _ -> closed_with_error "Unexpected roster get during sasl negotiation"
  | ROSTER_SET _ -> closed_with_error "Unexpected roster set during sasl negotiation"
  | ROSTER_REMOVE _ ->
    closed_with_error "Unexpected roster remove during sasl negotiation"
  | SUBSCRIPTION_REQUEST _ ->
    closed_with_error "Unexpected subscription request during sasl negotiation"
  | PRESENCE_UPDATE _ ->
    closed_with_error "Unexpected presence update during sasl negotiation"
  | IQ_ERROR {error_type; error_tag; id} ->
    {state = SASL_NEGOTIATION}, [Actions.IQ_ERROR {error_type; error_tag; id}], []
  | MESSAGE _ -> closed_with_error "Unexpected message during sasl negotiation"
  | LOG_OUT ->
    closed_with_error "Unexpected presence for log out during sasl negotiation"
  | NOOP -> t, [], []
  | SUBSCRIPTION_APPROVAL _ ->
    closed_with_error "Unexpected subscription approval during sasl negotiation"
;;

let handle_negotiating t = function
  | STREAM_HEADER {ato; version} ->
    if ato <> Jid.Empty
    then
      if float_of_string version >= 1.0
      then
        ( {state = NEGOTIATING}
        , [Actions.SEND_STREAM_HEADER; Actions.SEND_STREAM_FEATURES]
        , [] )
      else closed_with_error "Must use version >= 1.0"
    else closed_with_error "Received empty to attribute"
  | SASL_AUTH _ -> closed_with_error "Already negotiated sasl"
  | RESOURCE_BIND_SERVER_GEN id ->
    {state = NEGOTIATING}, [Actions.SERVER_GEN_RESOURCE_IDENTIFIER id], []
  | RESOURCE_BIND_CLIENT_GEN {id; resource} ->
    {state = NEGOTIATING}, [Actions.SET_JID_RESOURCE {id; resource}], []
  | SESSION_START id -> {state = CONNECTED}, [Actions.SESSION_START_SUCCESS id], []
  | STREAM_CLOSE ->
    (* the stream can close during negotiation so close our direction too *)
    closed
  | ERROR e -> closed_with_error e
  | ROSTER_GET id ->
    {state = CONNECTED}, [Actions.ADD_TO_CONNECTIONS; Actions.GET_ROSTER id], []
  | ROSTER_SET {id; target; handle; groups} ->
    ( {state = CONNECTED}
    , [ Actions.ADD_TO_CONNECTIONS
      ; Actions.SET_ROSTER {id; target; handle; groups}
      ; Actions.PUSH_ROSTER {ato = None; contact = target} ]
    , [] )
  | ROSTER_REMOVE {id; target} ->
    ( {state = CONNECTED}
    , [ Actions.ROSTER_REMOVE {id; target}
      ; Actions.PUSH_ROSTER {ato = None; contact = target} ]
    , [] )
  | SUBSCRIPTION_REQUEST {ato; xml} ->
    ( {state = CONNECTED}
    , [ Actions.SUBSCRIPTION_REQUEST {ato; xml; from = None}
      ; Actions.PUSH_ROSTER {ato = None; contact = ato} ]
    , [] )
  | PRESENCE_UPDATE available ->
    {state = CONNECTED}, [Actions.UPDATE_PRESENCE available], []
  | IQ_ERROR {error_type; error_tag; id} ->
    {state = NEGOTIATING}, [Actions.IQ_ERROR {error_type; error_tag; id}], []
  | MESSAGE {ato; message} -> {state = CONNECTED}, [Actions.MESSAGE {ato; message}], []
  | LOG_OUT -> closed
  | NOOP -> t, [], []
  | SUBSCRIPTION_APPROVAL {ato; xml} ->
    ( {state = CONNECTED}
    , [ Actions.SUBSCRIPTION_APPROVAL {ato; xml; from = None}
      ; Actions.ROSTER_SET_FROM ato
      ; Actions.PUSH_ROSTER {ato = None; contact = ato}
      ; Actions.SEND_CURRENT_PRESENCE ato ]
    , [] )
;;

let handle_connected t = function
  | STREAM_HEADER _ -> closed_with_error "Not expecting stream header"
  | SASL_AUTH _ -> closed_with_error "Already negotiated sasl"
  | RESOURCE_BIND_SERVER_GEN _ -> closed_with_error "Already connected"
  | RESOURCE_BIND_CLIENT_GEN _ -> closed_with_error "Already connected"
  | SESSION_START id -> {state = CONNECTED}, [Actions.SESSION_START_SUCCESS id], []
  | STREAM_CLOSE -> closed
  | ERROR e -> closed_with_error e
  | ROSTER_GET id -> {state = CONNECTED}, [Actions.GET_ROSTER id], []
  | ROSTER_SET {id; target; handle; groups} ->
    ( {state = CONNECTED}
    , [ Actions.SET_ROSTER {id; target; handle; groups}
      ; Actions.PUSH_ROSTER {ato = None; contact = target} ]
    , [] )
  | ROSTER_REMOVE {id; target} ->
    ( {state = CONNECTED}
    , [ Actions.ROSTER_REMOVE {id; target}
      ; Actions.PUSH_ROSTER {ato = None; contact = target} ]
    , [] )
  | SUBSCRIPTION_REQUEST {ato; xml} ->
    ( {state = CONNECTED}
    , [ Actions.SUBSCRIPTION_REQUEST {ato; xml; from = None}
      ; Actions.PUSH_ROSTER {ato = None; contact = ato} ]
    , [] )
  | PRESENCE_UPDATE available ->
    {state = CONNECTED}, [Actions.UPDATE_PRESENCE available], []
  | IQ_ERROR {error_type; error_tag; id} ->
    {state = CONNECTED}, [Actions.IQ_ERROR {error_type; error_tag; id}], []
  | MESSAGE {ato; message} -> {state = CONNECTED}, [Actions.MESSAGE {ato; message}], []
  | LOG_OUT -> closed
  | NOOP -> t, [], []
  | SUBSCRIPTION_APPROVAL {ato; xml} ->
    ( {state = CONNECTED}
    , [ Actions.SUBSCRIPTION_APPROVAL {ato; xml; from = None}
      ; Actions.ROSTER_SET_FROM ato
      ; Actions.PUSH_ROSTER {ato = None; contact = ato}
      ; Actions.SEND_CURRENT_PRESENCE ato ]
    , [] )
;;

let handle_closed t = function
  | STREAM_HEADER _s -> closed_with_error "Not expecting stream header"
  | SASL_AUTH _ -> closed_with_error "Already negotiated sasl"
  | RESOURCE_BIND_SERVER_GEN _ -> closed_with_error "Connection closed"
  | RESOURCE_BIND_CLIENT_GEN _ -> closed_with_error "Connection closed"
  | SESSION_START _id -> closed_with_error "Not expecting session start"
  | STREAM_CLOSE ->
    (* shouldn't receive another close after being closed *)
    closed_with_error "Not expecting a close"
  | ERROR e -> closed_with_error e
  | ROSTER_GET _ -> closed_with_error "already closed"
  | ROSTER_SET _ -> closed_with_error "already closed"
  | ROSTER_REMOVE _ -> closed_with_error "already closed"
  | SUBSCRIPTION_REQUEST _ -> closed_with_error "already closed"
  | PRESENCE_UPDATE _ -> closed_with_error "already closed"
  | IQ_ERROR _ -> closed_with_error "already closed"
  | MESSAGE _ -> closed_with_error "already closed"
  | LOG_OUT -> closed
  | NOOP -> t, [], []
  | SUBSCRIPTION_APPROVAL _ -> closed_with_error "already closed"
;;

let handle t event =
  match t.state with
  | IDLE -> handle_idle t event
  | SASL_NEGOTIATION -> handle_sasl_negotiation t event
  | NEGOTIATING -> handle_negotiating t event
  | CONNECTED -> handle_connected t event
  | CLOSED -> handle_closed t event
;;

let%expect_test "create" =
  let fsm = initial in
  print_endline (to_string fsm);
  [%expect {| {state: IDLE} |}]
;;

let%expect_test "idle to negotiating" =
  let fsm = initial in
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.STREAM_HEADER {ato = Jid.of_string "im.example.com"; version = "1.0"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: SASL_NEGOTIATION} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SEND_STREAM_HEADER
    SEND_STREAM_FEATURES_SASL |}]
;;

let%expect_test "idle to negotiating with > 1.0" =
  let fsm = initial in
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.STREAM_HEADER {ato = Jid.of_string "im.example.com"; version = "2.0"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: SASL_NEGOTIATION} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SEND_STREAM_HEADER
    SEND_STREAM_FEATURES_SASL |}]
;;

let%expect_test "negotiating to closing" =
  let fsm = initial in
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.STREAM_HEADER {ato = Jid.of_string "im.example.com"; version = "1.0"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: SASL_NEGOTIATION} |}];
  let strings = List.map (fun a -> Utils.mask_id @@ Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SEND_STREAM_HEADER
    SEND_STREAM_FEATURES_SASL |}];
  let fsm, actions, _handler_actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: CLOSED} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    UPDATE_PRESENCE: availability=Offline
    REMOVE_FROM_CONNECTIONS
    ERROR: Unexpected stream close during sasl negotiation |}]
;;

let%expect_test "sasl negotiation" =
  let fsm = initial in
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.STREAM_HEADER {ato = Jid.of_string "im.example.com"; version = "1.0"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: SASL_NEGOTIATION} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SEND_STREAM_HEADER
    SEND_STREAM_FEATURES_SASL |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.SASL_AUTH {user = "juliet"; password = ""})
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SET_JID: juliet
    SEND_SASL_SUCCESS |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.RESOURCE_BIND_SERVER_GEN "id")
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (fun s -> print_endline s) strings;
  [%expect {|
    SERVER_GEN_RESOURCE_IDENTIFIER: id |}];
  let fsm, actions, _handler_actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: CLOSED} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    UPDATE_PRESENCE: availability=Offline
    REMOVE_FROM_CONNECTIONS
    CLOSE |}]
;;

let%expect_test "bind resource" =
  let fsm = initial in
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.STREAM_HEADER {ato = Jid.of_string "im.example.com"; version = "1.0"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: SASL_NEGOTIATION} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SEND_STREAM_HEADER
    SEND_STREAM_FEATURES_SASL |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.SASL_AUTH {user = "juliet"; password = ""})
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SET_JID: juliet
    SEND_SASL_SUCCESS |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.RESOURCE_BIND_SERVER_GEN "id")
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (fun s -> print_endline s) strings;
  [%expect {|
    SERVER_GEN_RESOURCE_IDENTIFIER: id |}];
  let fsm, actions, _handler_actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: CLOSED} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    UPDATE_PRESENCE: availability=Offline
    REMOVE_FROM_CONNECTIONS
    CLOSE |}]
;;

let%expect_test "bind resource client" =
  let fsm = initial in
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.STREAM_HEADER {ato = Jid.of_string "im.example.com"; version = "1.0"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: SASL_NEGOTIATION} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SEND_STREAM_HEADER
    SEND_STREAM_FEATURES_SASL |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.SASL_AUTH {user = "juliet"; password = ""})
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SET_JID: juliet
    SEND_SASL_SUCCESS |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.RESOURCE_BIND_CLIENT_GEN {id = "id"; resource = "client-res"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (fun s -> print_endline s) strings;
  [%expect {|
    SET_JID_RESOURCE: id=id resource=client-res |}];
  let fsm, actions, _handler_actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: CLOSED} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect
    {|
    UPDATE_PRESENCE: availability=Offline
    REMOVE_FROM_CONNECTIONS
    CLOSE |}]
;;

let%expect_test "roster get" =
  let fsm = initial in
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.STREAM_HEADER {ato = Jid.of_string "im.example.com"; version = "1.0"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: SASL_NEGOTIATION} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect {|
    SEND_STREAM_HEADER
    SEND_STREAM_FEATURES_SASL |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.SASL_AUTH {user = "juliet"; password = ""})
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SET_JID: juliet
    SEND_SASL_SUCCESS |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.RESOURCE_BIND_CLIENT_GEN {id = "id"; resource = "client-res"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect {| SET_JID_RESOURCE: id=id resource=client-res |}];
  let fsm, actions, _handler_actions = handle fsm (Events.ROSTER_GET "some_id") in
  print_endline (to_string fsm);
  [%expect {| {state: CONNECTED} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect {|
    ADD_TO_CONNECTIONS
    GET_ROSTER: id=some_id |}];
  let fsm, actions, _handler_actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: CLOSED} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect
    {|
    UPDATE_PRESENCE: availability=Offline
    REMOVE_FROM_CONNECTIONS
    CLOSE |}]
;;

let%expect_test "roster set" =
  let fsm = initial in
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.STREAM_HEADER {ato = Jid.of_string "im.example.com"; version = "1.0"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: SASL_NEGOTIATION} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect {|
    SEND_STREAM_HEADER
    SEND_STREAM_FEATURES_SASL |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.SASL_AUTH {user = "juliet"; password = ""})
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  let strings = List.map (fun a -> Actions.to_string a) actions in
  List.iter (Printf.printf "%s\n") strings;
  [%expect {|
    SET_JID: juliet
    SEND_SASL_SUCCESS |}];
  let fsm, actions, _handler_actions =
    handle fsm (Events.RESOURCE_BIND_CLIENT_GEN {id = "id"; resource = "client-res"})
  in
  print_endline (to_string fsm);
  [%expect {| {state: NEGOTIATING} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect {| SET_JID_RESOURCE: id=id resource=client-res |}];
  let fsm, actions, _handler_actions = handle fsm (Events.ROSTER_GET "some_id") in
  print_endline (to_string fsm);
  [%expect {| {state: CONNECTED} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect {|
    ADD_TO_CONNECTIONS
    GET_ROSTER: id=some_id |}];
  let fsm, actions, _handler_actions =
    handle
      fsm
      (Events.ROSTER_SET
         { id = "some_id"
         ; target = Jid.of_string "nurse@example.com"
         ; handle = "Nurse"
         ; groups = ["Servants"] })
  in
  print_endline (to_string fsm);
  [%expect {| {state: CONNECTED} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (fun s -> print_endline s);
  [%expect
    {|
    SET_ROSTER: id=some_id target=nurse@example.com handle=Nurse groups=[Servants]
    PUSH_ROSTER: to=None contact=nurse@example.com] |}];
  let fsm, actions, _handler_actions = handle fsm Events.STREAM_CLOSE in
  print_endline (to_string fsm);
  [%expect {| {state: CLOSED} |}];
  List.map (fun a -> Actions.to_string a) actions |> List.iter (Printf.printf "%s\n");
  [%expect
    {|
    UPDATE_PRESENCE: availability=Offline
    REMOVE_FROM_CONNECTIONS
    CLOSE |}]
;;
