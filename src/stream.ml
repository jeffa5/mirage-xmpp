type t =
  | Header of Xml.tag
  | Features
  | Error
  | Close
[@@deriving sexp]

let to_string = function
  | Header tag -> Xml.tag_to_string ~empty:false tag
  | Features -> "features"
  | Error -> "error"
  | Close -> "</stream:stream>"
;;

let features_sasl_mechanisms =
  Xml.create
    (("stream", "features"), [])
    ~children:
      [ Xml.create
          (("", "mechanisms"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-sasl"])
          ~children:
            [ Xml.create (("", "mechanism"), []) ~children:[Xml.Text "PLAIN"]
            ; Xml.create (("", "mechanism"), []) ~children:[Xml.Text "ANONYMOUS"] ] ]
;;

let features =
  Xml.create
    (("stream", "features"), [])
    ~children:
      [Xml.create (("", "bind"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-bind"])]
;;

let create_header
    ?(version = "1.0")
    ?(lang = "en")
    ?(xmlns = "jabber:client")
    ?(stream_ns = "http://etherx.jabber.org/streams")
    ?(attributes = [])
    ?ato
    ?from
    () =
  let attributes =
    match ato with Some v -> ("", Xml.To v) :: attributes | None -> attributes
  in
  let attributes =
    match from with Some v -> ("", Xml.From v) :: attributes | None -> attributes
  in
  ( ("stream", "stream")
  , [ "", Xml.Id (Stanza.gen_id ())
    ; "", Xml.Version version
    ; "xml", Xml.Lang lang
    ; "", Xml.Xmlns xmlns
    ; "xmlns", Xml.Stream stream_ns ]
    @ attributes )
;;
