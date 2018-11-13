type t =
  { connections : Connections.t ref
  ; roster : Roster.t
  ; parser : Parser.t
  ; callback : string option -> unit
  ; jid : Jid.t
  ; mutable fsm : State.t
  ; actions_push : Actions.t option -> unit }

let handle_action t stream =
  let rec aux () =
    match%lwt Lwt_stream.get stream with
    | Some action ->
      let open Actions in
      (match action with
      | REPLY_STANZA (b, s) -> t.callback (Some (Stanza.to_string ~auto_close:b s))
      | SEND_STANZA (_jid, s) -> t.callback (Some (Stanza.to_string s)));
      aux ()
    | None ->
      t.callback None;
      Lwt.return_unit
  in
  aux ()
;;

let create ~connections ~roster ~stream ~callback =
  let parser = Parser.create stream in
  let jid = Jid.empty in
  let fsm = State.create () in
  let actions_stream, actions_push = Lwt_stream.create () in
  let t = {connections; roster; parser; callback; jid; fsm; actions_push} in
  Lwt.async (fun () -> handle_action t actions_stream);
  t
;;

exception HandlerException of string
exception EndOfStream

let handle t =
  let rec aux () =
    let%lwt stanza =
      match%lwt Parser.parse_stanza t.parser with
      | Ok s ->
        (match s with Some stanza -> Lwt.return stanza | None -> Lwt.fail EndOfStream)
      | Error e -> Lwt.fail (HandlerException e)
    in
    let event = Events.STANZA stanza in
    let new_fsm, actions = State.update t.fsm event in
    t.fsm <- new_fsm;
    List.iter (fun action -> t.actions_push (Some action)) actions;
    aux ()
  in
  aux ()
;;

let to_string t =
  "{\n"
  ^ "roster: "
  ^ Roster.to_string t.roster
  ^ "\n"
  ^ "parser: "
  ^ Parser.to_string t.parser
  ^ "\n"
  ^ "callback"
  ^ "\n"
  ^ "jid: "
  ^ Jid.to_string t.jid
  ^ "\n"
  ^ "fsm: "
  ^ State.to_string t.fsm
  ^ "\n"
  ^ "}"
;;

let make_test_handler s =
  let mask_id s =
    match Astring.String.find_sub ~sub:"id='" s with
    | Some i ->
      (match Astring.String.find_sub ~start:(i + 4) ~sub:"'" s with
      | Some j ->
        Astring.String.with_index_range ~first:0 ~last:(i + 3) s
        ^ "redacted_for_testing"
        ^ Astring.String.with_index_range ~first:j s
      | None -> assert false)
    | None -> s
  in
  let connections = ref Connections.empty in
  let roster = Roster.empty in
  let stream = Lwt_stream.of_string s in
  let callback so = match so with Some s -> print_endline (mask_id s) | None -> () in
  create ~connections ~roster ~stream ~callback
;;

let test_stanza stanza =
  let handler = make_test_handler stanza in
  let run = handle handler in
  try Lwt_main.run run with EndOfStream -> print_endline "end_of_stream"
;;

let%expect_test "creation of handler" =
  let handler = make_test_handler "<stream></stream>" in
  print_endline (to_string handler);
  [%expect
    {|
    {
    roster: []
    parser: stream and depth
    callback
    jid: empty
    fsm: {state: idle}
    } |}]
;;

let%expect_test "initial stanza event" =
  let stanza =
    Stanza.to_string
      ~auto_close:false
      (Stanza.create
         ( ("", "stream")
         , [("", "from"), "juliet@im.example.com"; ("", "to"), "im.example.com"] ))
    ^ "</stream>"
  in
  test_stanza stanza;
  [%expect
    {|
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    end_of_stream |}]
;;

let%expect_test "initial stanza with version" =
  let stanza =
    Stanza.to_string
      ~auto_close:false
      (Stanza.create
         ( ("stream", "stream")
         , [ ("", "from"), "juliet@im.example.com"
           ; ("", "to"), "im.example.com"
           ; ("", "version"), "1.0"
           ; ("xml", "lang"), "en"
           ; ("", "xmlns"), "jabber:client"
           ; ("xmlns", "stream"), "http://etherx.jabber.org/streams" ] ))
    ^ "</stream:stream>"
  in
  test_stanza stanza;
  [%expect
    {|
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    <stream:features/>
    end_of_stream |}]
;;
