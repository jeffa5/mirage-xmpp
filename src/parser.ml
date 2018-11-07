type t = (Markup.signal, Markup.async) Markup.stream

exception ParsingError of string

let make_parser stream =
  Markup_lwt.parse_xml
    ~report:(fun _ e ->
        let error_string = Markup.Error.to_string e in
        Lwt.fail (ParsingError error_string) )
    stream
;;

let create stream =
  let signals = Markup_lwt.lwt_stream stream |> make_parser |> Markup.signals in
  signals
;;

let parse_stanza _ =
  Stanza.create (("", "name"), [])