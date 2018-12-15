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
      [Xml.create (("", "bind"), [("", "xmlns"), "urn:ietf:params:xml:ns:xmpp-bind"])]
;;
