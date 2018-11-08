type t =
  { stream : (Markup.signal, Markup.async) Markup.stream
  ; mutable depth : int }

exception ParsingError of string

let make_parser stream =
  Markup_lwt.parse_xml
    ~report:(fun _ e ->
      let error_string = Markup.Error.to_string e in
      Lwt.fail (ParsingError error_string) )
    stream
;;

let create stream =
  let stream = Markup_lwt.lwt_stream stream |> make_parser |> Markup.signals in
  {stream; depth = 0}
;;

let parse_stanza parser =
  let rec aux () =
    let stanza = ref None in
    match%lwt Markup_lwt.next parser.stream with
    | exception ParsingError e -> Lwt.return_error e
    | Some signal ->
      (match signal with
      | `Start_element tag ->
        stanza := Some (Stanza.create tag);
        parser.depth <- parser.depth + 1;
        if parser.depth = 1
        then (* return initial stream stanza *)
          Lwt.return_ok !stanza
        else
          (* Parse all the children into this stanza *)
          let rec parse_child () =
            match%lwt aux () with
            | Ok (Some child) ->
              (match !stanza with
              | Some s ->
                stanza := Some (Stanza.add_content s child);
                parse_child ()
              | None -> assert false)
            | Ok None -> Lwt.return_ok !stanza
            | Error e -> Lwt.return_error e
          in
          parse_child ()
      | `End_element ->
        parser.depth <- parser.depth - 1;
        Lwt.return_ok !stanza
      | `Text s -> Lwt.return_ok (Some (Stanza.text s))
      | _ -> assert false)
    | None -> assert false
  in
  match%lwt aux () with
  | Ok (Some s) -> Lwt.return_ok s
  | Error e -> Lwt.return_error e
  | Ok None -> Lwt.return_error "Nothing left to parse"
;;

let parse_string s =
  let parser = create (Lwt_stream.of_string s) in
  let out () =
    match%lwt parse_stanza parser with
    | Ok s ->
      print_endline (Stanza.pp_to_string s);
      Lwt.return_unit
    | Error e ->
      print_endline e;
      Lwt.return_unit
  in
  fun () -> Lwt_main.run (out ())
;;

let%expect_test "initial stanza gets returned" =
  let pf = parse_string "<stream></stream>" in
  pf ();
  [%expect {| <stream /> |}]
;;

let%expect_test "second stanza should get returned too" =
  let pf = parse_string "<stream><second/></stream>" in
  pf ();
  [%expect {| <stream /> |}];
  pf ();
  [%expect {| <second /> |}]
;;

let%expect_test "nested stanza" =
  let pf = parse_string "<stream><body><message>A message!</message></body></stream>" in
  pf ();
  [%expect {| <stream /> |}];
  pf ();
  [%expect
    {|
    <body>
      <message>
        A message!
      </message>
    </body> |}]
;;
