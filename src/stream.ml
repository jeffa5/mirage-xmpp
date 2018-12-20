type t =
  | Header of Xml.tag
  | Features
  | Error
  | Close

let to_string = function
  | Header tag -> Xml.tag_to_string ~empty:false tag
  | Features -> "features"
  | Error -> "error"
  | Close -> "</stream:stream>"
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
    from
    dest =
  ( ("stream", "stream")
  , [ "", Xml.From from
    ; "", Xml.Id (Stanza.gen_id ())
    ; "", Xml.To dest
    ; "", Xml.Version version
    ; "xml", Xml.Lang lang
    ; "", Xml.Xmlns xmlns
    ; "xmlns", Xml.Stream stream_ns ]
    @ attributes )
;;
