open Async

let send ?(host = "10.0.0.2") ?(port = 8080) str =
  let host_and_port = Core.Host_and_port.create ~host ~port in
  let where_to_connect = Tcp.Where_to_connect.of_host_and_port host_and_port in
  let%bind s =
    Tcp.with_connection where_to_connect (fun _socket reader writer ->
        Writer.write writer str;
        Reader.contents reader )
  in
  printf "%s" s;
  Deferred.unit
;;

let%expect_test "no start tag" =
  let%bind () = send "</error>" in
  [%expect {| Error: bad document: expected root element |}]
;;

let%expect_test "unmatched end tag" =
  let%bind () = send "<start></a></start>" in
  [%expect {| Error: unmatched end tag 'a' |}]
;;

let%expect_test "unmatched start tag" =
  let%bind () = send "<start><a></start>" in
  [%expect {| Error: unmatched start tag 'a' |}]
;;

let%expect_test "simple open close" =
  let%bind () = send "<stream></stream>" in
  [%expect {| XML accepted. |}]
;;

let%expect_test "no closing tag" =
  let%bind () = send "<message><body>No closing tag!</message>" in
  [%expect {| Error: unmatched start tag 'body' |}]
;;

let%expect_test "xmpp initial" =
  let%bind () =
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
      \   </stream:stream>"
  in
  [%expect {| XML accepted. |}]
;;

let%expect_test "unknown namespace" =
  let%bind () =
    send
      "<stream:error>\n\
      \        <not-well-formed\n\
      \            xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>\n\
      \      </stream:error>\n\
      \      </stream:stream>"
  in
  [%expect {| Error: unknown namespace 'stream' |}]
;;

let%expect_test "xmpp initial extended" =
  let%bind () =
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
      \      </stream:stream>"
  in
  [%expect {| XML accepted. |}]
;;
