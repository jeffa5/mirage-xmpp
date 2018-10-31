open Mirage

let port =
  let doc =
    Key.Arg.info ~doc:"The TCP port on which to listen for incoming connections." ["port"]
  in
  Key.(create "port" Arg.(opt int 8080 doc))
;;

let packages =
  [package "markup-lwt"; package "core"; package "lwt_ppx"; package "xmlparser"]
;;

let main = foreign ~keys:[Key.abstract port] ~packages "Unikernel.Main" (stackv4 @-> job)
let stack = generic_stackv4 default_network
let () = register "xmlparse" [main $ stack]
