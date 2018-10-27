open Core_kernel

module Main (S : Mirage_stack_lwt.V4) = struct
  module Flowout = Flowout.Main (S)
  module Xml = Parser.Main (Flowout)

  let read flow pushf =
    let rec aux () =
      match%lwt S.TCPV4.read flow with
      | Ok `Eof | Error _ -> Lwt.return_unit
      | Ok (`Data b) ->
        let s = Cstruct.to_string b in
        String.iter ~f:(fun c -> pushf (Some c)) s;
        aux ()
    in
    aux ()
  ;;

  let on_connect flow =
    let stream, pushf = Lwt_stream.create () in
    Lwt.async (fun () -> read flow pushf);
    let%lwt () = Xml.parse_xml flow stream in
    Logs.info (fun f -> f "Closing the connection");
    S.TCPV4.close flow
  ;;

  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port on_connect;
    S.listen s
  ;;
end
