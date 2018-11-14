let send ?(timeout = 10.) ?(host = "10.0.0.2") ?(port = 8080) str =
  let timeout_t =
    let%lwt () = Lwt_unix.sleep timeout in
    Lwt.return "Timeout"
  in
  let request =
    let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
    Lwt_io.(
      with_connection addr (fun (_i, o) ->
          let%lwt () = write o str in
          Lwt.return "Success" ))
  in
  let s = Lwt_main.run (Lwt.pick [request; timeout_t]) in
  print_endline s
;;

let send_recv ?(timeout = 10.) ?(host = "10.0.0.2") ?(port = 8080) str =
  let timeout_t =
    let%lwt () = Lwt_unix.sleep timeout in
    Lwt.return "Timeout"
  in
  let request =
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
    let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
    Lwt_io.(
      with_connection addr (fun (i, o) ->
          print_endline "Send:";
          print_endline str;
          let%lwt () = write o str in
          print_endline "Receive:";
          let%lwt s = read_line i in
          Lwt.return (mask_id s) ))
  in
  let s = Lwt_main.run (Lwt.pick [request; timeout_t]) in
  print_endline s
;;

let send_recv_list ?(timeout = 10.) ?(host = "10.0.0.2") ?(port = 8080) l =
  List.iter (fun s -> send_recv ~timeout ~host ~port s) l
;;

let configure_tap () =
  print_endline "Configuring tap0";
  let command = Lwt_process.shell "ifconfig tap0 10.0.0.1 up" in
  match Lwt_main.run (Lwt_process.exec command) with _ -> ()
;;

let start_unikernel () =
  print_endline "Starting unikernel";
  let command =
    Lwt_process.shell
      "cd ../../../; sudo mirage/xmpp -l \"*:debug\" > unikernel.log 2>&1"
  in
  let _process = Lwt_process.open_process_none command in
  Unix.sleep 1;
  configure_tap ()
;;

let stop_unikernel () =
  print_endline "Stopping unikernel";
  send ~port:8081 "exit"
;;

let test_unikernel f =
  start_unikernel ();
  f ();
  stop_unikernel ()
;;

let%expect_test "start stop" =
  test_unikernel (fun () -> ());
  [%expect
    {|
    Starting unikernel
    Configuring tap0
    Stopping unikernel
    Success |}]
;;

let%expect_test "initial stanza" =
  test_unikernel (fun () ->
      send_recv
        "<stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' \
         xml:lang='en' xmlns='jabber:client' \
         xmlns:stream='http://etherx.jabber.org/streams'>" );
  [%expect
    {|
    Starting unikernel
    Configuring tap0
    Send:
    <stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    Receive:
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    Stopping unikernel
    Success |}]
;;

let%expect_test "initial stanza in list" =
  test_unikernel (fun () ->
      send_recv_list
        [ "<stream:stream from='juliet@im.example.com' to='im.example.com' \
           version='1.0' xml:lang='en' xmlns='jabber:client' \
           xmlns:stream='http://etherx.jabber.org/streams'>" ] );
  [%expect
    {|
    Starting unikernel
    Configuring tap0
    Send:
    <stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    Receive:
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    Stopping unikernel
    Success |}]
;;
