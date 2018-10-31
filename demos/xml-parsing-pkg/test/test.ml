open Core_kernel

exception Timeout

let send ?(timeout = 5.) ?(host = "10.0.0.2") ?(port = 8080) str =
  let timeout_t =
    let%lwt () = Lwt_unix.sleep timeout in
    Lwt.fail Timeout
  in
  let request =
    let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
    Lwt_io.(
      with_connection addr (fun (i, o) ->
          let%lwt () = write o str in
          read i ))
  in
  let s = Lwt_main.run (Lwt.pick [request; timeout_t]) in
  print_endline s
;;

let%expect_test "no start tag" =
  send "</error>";
  [%expect {| Error: bad document: expected root element |}]
;;

let%expect_test "unmatched end tag" =
  send "<start></a></start>";
  [%expect {| Error: unmatched end tag 'a' |}]
;;

let%expect_test "unmatched start tag" =
  send "<start><a></start>";
  [%expect {| Error: unmatched start tag 'a' |}]
;;

let%expect_test "simple open close" =
  send "<stream></stream>";
  [%expect {| XML accepted. |}]
;;

let%expect_test "no closing tag" =
  send "<message><body>No closing tag!</message>";
  [%expect {| Error: unmatched start tag 'body' |}]
;;

let%expect_test "xmpp initial" =
  send
    "<stream:stream\n\
    \       from='juliet@im.example.com'\n\
    \       to='im.example.com'\n\
    \       version='1.0'\n\
    \       xml:lang='en'\n\
    \       xmlns='jabber:client'\n\
    \       xmlns:stream='http://etherx.jabber.org/streams'>\n\
    \     <message>\n\
    \       <body>foo</body>\n\
    \     </message>\n\
    \   </stream:stream>";
  [%expect {| XML accepted. |}]
;;

let%expect_test "unknown namespace" =
  send
    "<stream:error>\n\
    \        <not-well-formed\n\
    \            xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>\n\
    \      </stream:error>\n\
    \      </stream:stream>";
  [%expect {| Error: unknown namespace 'stream' |}]
;;

let%expect_test "xmpp initial extended" =
  send
    "<?xml version='1.0'?>\n\
    \      <stream:stream\n\
    \          from='im.example.com'\n\
    \          id='++TR84Sm6A3hnt3Q065SnAbbk3Y='\n\
    \          to='juliet@im.example.com'\n\
    \          version='1.0'\n\
    \          xml:lang='en'\n\
    \          xmlns='jabber:client'\n\
    \          xmlns:stream='http://etherx.jabber.org/streams'>\n\
    \      <stream:error>\n\
    \        <invalid-namespace\n\
    \            xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>\n\
    \      </stream:error>\n\
    \      </stream:stream>";
  [%expect {| XML accepted. |}]
;;
