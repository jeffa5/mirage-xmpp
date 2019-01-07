open Mirage

let port =
  let doc =
    Key.Arg.info ~doc:"The TCP port on which to listen for incoming connections." ["port"]
  in
  Key.(create "port" Arg.(opt int 5222 doc))
;;

let hostname =
  let doc = Key.Arg.info ~doc:"The hostname for the server." ["hostname"] in
  Key.(create "hostname" Arg.(opt string "mirage-xmpp.dev" doc))
;;

let packages = [package "lwt_ppx"; package "mirage-xmpp"]

let main =
  foreign
    ~keys:[Key.abstract port; Key.abstract hostname]
    ~packages
    "Unikernel.Main"
    (stackv4 @-> job)
;;

let stack = generic_stackv4 default_network
let () = register "xmpp" [main $ stack]
