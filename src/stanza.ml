type t =
  | Message of Xml.t
  | Presence of Xml.t
  | Iq of Xml.t
[@@deriving sexp]

let to_xml = function Message xml -> xml | Presence xml -> xml | Iq xml -> xml
let gen_id () = Uuidm.(to_string (create `V4))

let create_presence ?(attributes = []) ?atype ?ato ~id ~from children =
  let attributes =
    match atype with Some t -> ("", Xml.Type t) :: attributes | None -> attributes
  in
  let attributes =
    match id with Some i -> ("", Xml.Id i) :: attributes | None -> attributes
  in
  let attributes =
    match ato with Some ato -> ("", Xml.To ato) :: attributes | None -> attributes
  in
  Presence (Element ((("", "presence"), ["", Xml.From from] @ attributes), children))
;;

let create_iq ?(attributes = []) ?ato ~atype ~id children =
  let attributes =
    match ato with Some ato -> ("", Xml.To ato) :: attributes | None -> attributes
  in
  Iq (Element ((("", "iq"), ["", Xml.Id id; "", Xml.Type atype] @ attributes), children))
;;

let create_iq_error ~from ~ato ~id ~error_type ~error_tag =
  Iq
    (Element
       ( ( ("", "iq")
         , ["", Xml.From from; "", Xml.Id id; "", Xml.To ato; "", Xml.Type "error"] )
       , [ Element
             ( (("", "error"), ["", Xml.Type (Actions.error_type_to_string error_type)])
             , [ Element
                   ( ( ("", error_tag)
                     , ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-stanzas"] )
                   , [] ) ] ) ] ))
;;

let create_bind ?(attributes = []) children =
  Xml.create
    (("", "bind"), ["", Xml.Xmlns "urn:ietf:params:xml:ns:xmpp-bind"] @ attributes)
    ~children
;;

let create_query children =
  Xml.create (("", "query"), ["", Xml.Xmlns "jabber:iq:roster"]) ~children
;;

let create_resource ?(attributes = []) children =
  Xml.create (("", "resource"), attributes) ~children
;;

let create_bind_result ~id ~jid () =
  create_iq
    ~id
    ~atype:"result"
    [create_bind [Xml.create (("", "jid"), []) ~children:[Xml.Text (Jid.to_string jid)]]]
;;

let create_roster_get_result ~id ~ato items =
  create_iq
    ~id
    ~atype:"result"
    ~ato
    [ create_query
        (List.map
           (fun (jid, item) ->
             let handle, subscription, _ask, groups = Rosters.Item.to_tuple item in
             Xml.create
               ( ("", "item")
               , [ "", Xml.Jid jid
                 ; "", Xml.Name handle
                 ; "", Xml.Subscription (Rosters.Subscription.to_string subscription) ]
               )
               ~children:
                 (List.map
                    (fun group ->
                      Xml.create (("", "group"), []) ~children:[Xml.Text group] )
                    groups) )
           items) ]
;;

let create_roster_set_result ~id ~ato = create_iq ~id ~atype:"result" ~ato []

let create_roster_push ~id ~ato (jid, item) =
  let handle, subscription, _ask, groups = Rosters.Item.to_tuple item in
  let attributes = [] in
  let attributes =
    match handle with "" -> attributes | h -> ("", Xml.Name h) :: attributes
  in
  let attributes =
    ("", Xml.Subscription (Rosters.Subscription.to_string subscription)) :: attributes
  in
  create_iq
    ~id
    ~ato
    ~atype:"set"
    [ create_query
        [ Xml.create
            (("", "item"), ["", Xml.Jid jid] @ attributes)
            ~children:
              (List.map
                 (fun group -> Xml.create (("", "group"), []) ~children:[Xml.Text group])
                 groups) ] ]
;;

let rec get_subscription = function
  | [] -> None
  | (_, Xml.Subscription sub) :: _ -> Some sub
  | _ :: attrs -> get_subscription attrs
;;

let rec get_id_exn = function
  | [] -> raise Not_found
  | (_, Xml.Id id) :: _ -> id
  | _ :: attrs -> get_id_exn attrs
;;

let rec get_id = function
  | [] -> None
  | (_, Xml.Id id) :: _ -> Some id
  | _ :: attrs -> get_id attrs
;;

let rec get_from = function
  | [] -> raise Not_found
  | (_, Xml.From jid) :: _ -> jid
  | _ :: attrs -> get_from attrs
;;

let rec get_to = function
  | [] -> raise Not_found
  | (_, Xml.To jid) :: _ -> jid
  | _ :: attrs -> get_to attrs
;;

let rec get_type = function
  | [] -> None
  | (_, Xml.Type t) :: _ -> Some t
  | _ :: attrs -> get_type attrs
;;

let rec get_version = function
  | [] -> raise Not_found
  | (_, Xml.Version v) :: _ -> v
  | _ :: attrs -> get_version attrs
;;

let rec get_jid = function
  | [] -> raise Not_found
  | (_, Xml.Jid jid) :: _ -> jid
  | _ :: attrs -> get_jid attrs
;;

let rec get_name = function
  | [] -> None
  | (_, Xml.Name name) :: _ -> Some name
  | _ :: attrs -> get_name attrs
;;

let to_string = function
  | Message xml -> Xml.to_string xml
  | Presence xml -> Xml.to_string xml
  | Iq xml -> Xml.to_string xml
;;
