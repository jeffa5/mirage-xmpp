let send ?(timeout = 10.) ?(host = "10.0.0.2") ?(port = 5222) str =
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

let send_recv ?(timeout = 10.) ?(host = "10.0.0.2") ?(port = 5222) str_list =
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
          let rec reader () =
            (* Repeatedly read data from the connection and print it *)
            let%lwt s = read_line i in
            print_endline ("Receive:\n" ^ mask_id s);
            if s = "</stream:stream>" then Lwt.return "Finished" else reader ()
          in
          let rec writer = function
            (* Send all the data in the list to the server *)
            | [] -> Lwt.return "Finished"
            | x :: xs ->
              print_endline ("Send:\n" ^ x);
              let%lwt () = write o x in
              let%lwt () = Lwt_unix.sleep 1. in
              writer xs
          in
          Lwt.async (fun () -> writer str_list);
          reader () ))
  in
  let s = Lwt_main.run (Lwt.pick [request; timeout_t]) in
  print_endline s
;;

let configure_tap () =
  print_endline "Configuring tap0";
  let command =
    Lwt_process.shell "sudo ip addr add 10.0.0.1/16 dev tap0 && sudo ip link set tap0 up"
  in
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

let%expect_test "initial stanza in list" =
  test_unikernel (fun () ->
      send_recv
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
    Receive:
    <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
    Timeout
    Stopping unikernel
    Success |}]
;;

let%expect_test "close stream" =
  test_unikernel (fun () ->
      send_recv
        [ "<stream:stream from='juliet@im.example.com' to='im.example.com' \
           version='1.0' xml:lang='en' xmlns='jabber:client' \
           xmlns:stream='http://etherx.jabber.org/streams'>"
        ; "</stream:stream>" ] );
  [%expect
    {|
    Starting unikernel
    Configuring tap0
    Send:
    <stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    Receive:
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    Receive:
    <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
    Send:
    </stream:stream>
    Receive:
    Closing the connection
    Receive:
    </stream:stream>
    Finished
    Stopping unikernel
    Success |}]
;;

let%expect_test "open stream with iq bind" =
  test_unikernel (fun () ->
      send_recv
        [ "<stream:stream from='juliet@im.example.com' to='im.example.com' \
           version='1.0' xml:lang='en' xmlns='jabber:client' \
           xmlns:stream='http://etherx.jabber.org/streams'>"
        ; "<iq id='yhc13a95' type='set'><bind \
           xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>balcony</resource></bind></iq>"
        ; "</stream:stream>" ] );
  [%expect
    {|
      Starting unikernel
      Configuring tap0
      Send:
      <stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
      Receive:
      <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
      Receive:
      <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
      Send:
      <iq id='yhc13a95' type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>balcony</resource></bind></iq>
      Receive:
      <iq id='redacted_for_testing' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
      Send:
      </stream:stream>
      Receive:
      Closing the connection
      Receive:
      </stream:stream>
      Finished
      Stopping unikernel
      Success |}]
;;

let%expect_test "open stream with iq bind and roster get without contacts" =
  test_unikernel (fun () ->
      send_recv
        [ "<stream:stream from='juliet@im.example.com' to='im.example.com' \
           version='1.0' xml:lang='en' xmlns='jabber:client' \
           xmlns:stream='http://etherx.jabber.org/streams'>"
        ; "<iq id='yhc13a95' type='set'><bind \
           xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>balcony</resource></bind></iq>"
        ; "<iq from='juliet@example.com/balcony' id='bv1bs71f' type='get'><query \
           xmlns='jabber:iq:roster'/></iq>"
        ; "</stream:stream>" ] );
  [%expect
    {|
      Starting unikernel
      Configuring tap0
      Send:
      <stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
      Receive:
      <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
      Receive:
      <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
      Send:
      <iq id='yhc13a95' type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>balcony</resource></bind></iq>
      Receive:
      <iq id='redacted_for_testing' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
      Send:
      <iq from='juliet@example.com/balcony' id='bv1bs71f' type='get'><query xmlns='jabber:iq:roster'/></iq>
      Receive:
      <iq id='redacted_for_testing' to='juliet@example.com/balcony' type='result'><query xmlns='jabber:iq:roster' ver='ver7'/></iq>
      Send:
      </stream:stream>
      Receive:
      Closing the connection
      Receive:
      </stream:stream>
      Finished
      Stopping unikernel
      Success |}]
;;

let%expect_test "open stream with iq bind and roster get with contacts" =
  test_unikernel (fun () ->
      send_recv
        [ "<stream:stream from='juliet@im.example.com' to='im.example.com' \
           version='1.0' xml:lang='en' xmlns='jabber:client' \
           xmlns:stream='http://etherx.jabber.org/streams'>"
        ; "<iq id='yhc13a95' type='set'><bind \
           xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>balcony</resource></bind></iq>"
        ; "<iq from='juliet@example.com/balcony' id='ph1xaz53' type='set'><query \
           xmlns='jabber:iq:roster'><item jid='nurse@example.com' \
           name='Nurse'><group>Servants</group></item></query></iq>"
        ; "<iq from='juliet@example.com/balcony' id='bv1bs71f' type='get'><query \
           xmlns='jabber:iq:roster'/></iq>"
        ; "</stream:stream>" ] );
  [%expect {|
    Starting unikernel
    Configuring tap0
    Send:
    <stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    Receive:
    <stream:stream from='im.example.com' id='redacted_for_testing' to='juliet@im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>
    Receive:
    <stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>
    Send:
    <iq id='yhc13a95' type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>balcony</resource></bind></iq>
    Receive:
    <iq id='redacted_for_testing' type='result'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>juliet@im.example.com/balcony</jid></bind></iq>
    Send:
    <iq from='juliet@example.com/balcony' id='ph1xaz53' type='set'><query xmlns='jabber:iq:roster'><item jid='nurse@example.com' name='Nurse'><group>Servants</group></item></query></iq>
    Receive:
    <iq id='redacted_for_testing' to='juliet@example.com/balcony' type='result'/>
    Send:
    <iq from='juliet@example.com/balcony' id='bv1bs71f' type='get'><query xmlns='jabber:iq:roster'/></iq>
    Receive:
    <iq id='redacted_for_testing' to='juliet@example.com/balcony' type='result'><query xmlns='jabber:iq:roster' ver='ver7'><item jid='nurse@example.com'/></query></iq>
    Send:
    </stream:stream>
    Receive:
    Closing the connection
    Receive:
    </stream:stream>
    Finished
    Stopping unikernel
    Success |}]
;;
