open Core_kernel

module type Main = sig
  type flow_type

  val write_string : flow_type -> Core.String.t -> unit Lwt.t
  val write_error_string : flow_type -> Core.String.t -> unit Lwt.t
end

module Main (S : Mirage_stack_lwt.V4) = struct
  type flow_type = S.TCPV4.flow

  let write_string flow s =
    let s = String.strip s ^ "\n" in
    let b = Cstruct.of_string s in
    match%lwt S.TCPV4.write flow b with
    | Ok () -> Lwt.return_unit
    | Error e ->
      Logs.warn (fun f ->
          f "Error occurred from writing to connection: %a" S.TCPV4.pp_write_error e );
      Lwt.return_unit
  ;;

  let write_error_string flow s =
    let s = "Error: " ^ String.strip s in
    write_string flow s
  ;;
end
