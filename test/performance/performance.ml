open Cmdliner

let run_python_stats file =
  let%lwt () = Lwt_io.printl @@ "PYTHON: Running stats on file: " ^ file in
  let%lwt _ = Lwt_unix.system @@ "python test/performance/stats.py --save " ^ file in
  Lwt_io.printl "PYTHON: Finished stats"
;;

let run_command_with_output command =
  let command = Lwt_process.shell command in
  let process = Lwt_process.open_process_in command in
  let rec get_lines () =
    match%lwt Lwt_io.read_line_opt process#stdout with
    | Some l ->
      let%lwt lines_after = get_lines () in
      Lwt.return ((string_of_float (Unix.gettimeofday ()) ^ " " ^ l) :: lines_after)
    | None -> Lwt.return_nil
  in
  get_lines ()
;;

let get_cpu_mem_docker container_name =
  let command =
    "docker stats --no-stream "
    ^ container_name
    ^ " | sed -n '2p' | awk '{gsub(/%/, \"\", $3); gsub(/%/, \"\", $7); printf \"%s \
       %s\", $3, $7}'"
  in
  run_command_with_output command
;;

let tsung file server_name =
  let tsung_command = Lwt_process.shell "tsung -f test/performance/tsung.xml start" in
  let tsung_process = Lwt_process.open_process_in tsung_command in
  let rec tsung_get_lines () =
    match%lwt Lwt_io.read_line_opt tsung_process#stdout with
    | Some l ->
      let%lwt () = Lwt_io.printl @@ "TSUNG: " ^ l in
      let%lwt lines_after = tsung_get_lines () in
      Lwt.return (l :: lines_after)
    | None -> Lwt.return_nil
  in
  let tsung_lines = ref [] in
  Lwt.async (fun () ->
      let%lwt tsung_output = tsung_get_lines () in
      tsung_lines := tsung_output;
      Lwt.return_unit );
  let cpumem_lines = ref [] in
  let rec main_loop () =
    match tsung_process#state with
    | Running ->
      let%lwt logged_lines = get_cpu_mem_docker server_name in
      cpumem_lines := !cpumem_lines @ logged_lines;
      let%lwt () = Lwt_unix.sleep 1. in
      main_loop ()
    | Exited _ -> Lwt.return_unit
  in
  let%lwt () = main_loop () in
  match%lwt tsung_process#status with
  | Unix.WEXITED 0 ->
    (* get the log file location *)
    let dump_time, dump_file =
      match
        List.filter
          (fun line -> Astring.String.is_prefix ~affix:"Log directory is:" line)
          !tsung_lines
      with
      | [line] ->
        (match Astring.String.cut ~sep:"/" line with
        | Some (_, path) ->
          ( (match Astring.String.cut ~rev:true ~sep:"/" path with
            | Some (_, datetime) -> datetime
            | None -> "")
          , "/" ^ path ^ "/tsung.dump" )
        | None -> "", "")
      | _ -> "", ""
    in
    (* separate the input file to the name of the xml file *)
    let config_name =
      match Astring.String.cut ~rev:true ~sep:"/" file with
      | Some (_, filename) ->
        (* remove the file extension *)
        (match Astring.String.cut ~rev:true ~sep:"." filename with
        | Some (fname, _) -> fname
        | None -> filename)
      | None -> file
    in
    (* copy the dump file to a new location with servername, xml file and time to identify it: servername-xmlfile-time.dump *)
    let results_dir =
      "test/performance/results/"
      ^ String.concat "-" [server_name; config_name; dump_time]
    in
    let%lwt () =
      try%lwt Lwt_unix.mkdir "test/performance/results" 0o755 with Unix.Unix_error _ ->
        Lwt.return_unit
    in
    let%lwt () =
      try%lwt Lwt_unix.mkdir results_dir 0o755 with Unix.Unix_error _ -> Lwt.return_unit
    in
    let%lwt cpumem_file = Lwt_io.open_file ~mode:Output (results_dir ^ "/cpumem") in
    let%lwt () =
      Lwt_list.iter_s (fun line -> Lwt_io.write_line cpumem_file line) !cpumem_lines
    in
    let%lwt () = Lwt_io.close cpumem_file in
    let copied_dump = results_dir ^ "/dump" in
    let%lwt () = Lwt_unix.rename dump_file copied_dump in
    let%lwt () = Lwt_io.printl "TSUNG: Finished Tsung" in
    Lwt.return results_dir
  | _ -> Lwt.return ""
;;

let test_docker image volume server_name file =
  let command =
    Lwt_process.shell
    @@ "docker run --rm --name "
    ^ server_name
    ^ " "
    ^ (if volume <> "" then "-v " ^ volume else "")
    ^ " -p 5222:5222 "
    ^ image
  in
  let%lwt () = Lwt_io.printl "DOCKER: Starting container" in
  let process = Lwt_process.open_process_in command in
  let rec get_lines () =
    match%lwt Lwt_io.read_line_opt process#stdout with
    | Some l ->
      let%lwt () = Lwt_io.printl @@ "DOCKER: " ^ l in
      get_lines ()
    | None -> Lwt.return_unit
  in
  Lwt.async get_lines;
  let%lwt () = Lwt_unix.sleep 20. in
  let%lwt results_dir = tsung file server_name in
  let%lwt () = Lwt_io.printl "DOCKER: Stopping container" in
  let%lwt _ = Lwt_unix.system @@ "docker stop " ^ server_name in
  Lwt.return results_dir
;;

let test_none server_name file = tsung file server_name

type server =
  | Mirage
  | Ejabberd
  | Tigase
  | Prosody
  | None

let server_to_string = function
  | Mirage -> "mirage"
  | Ejabberd -> "ejabberd"
  | Tigase -> "tigase"
  | Prosody -> "prosody"
  | None -> "none"
;;

let mirage = "jeffas/mirage-xmpp"
let ejabberd = "ejabberd/ecs"
let tigase = "dictcp/tigase"
let prosody = "prosody/prosody"

let performance
    servers files load_duration load_duration_unit load_arrivalrate load_arrivalrate_unit
    =
  Lwt_main.run
    (let%lwt () =
       Lwt_io.printl
       @@ "Servers: "
       ^ String.concat ", " (List.map (fun server -> server_to_string server) servers)
     in
     let%lwt () = Lwt_io.printl @@ "Files: " ^ String.concat ", " files in
     let%lwt () =
       Lwt_io.printl
       @@ "Load duration: "
       ^ string_of_int load_duration
       ^ " "
       ^ load_duration_unit
     in
     let%lwt () =
       Lwt_io.printl
       @@ "Load arrivalrate: "
       ^ string_of_int load_arrivalrate
       ^ " per "
       ^ load_arrivalrate_unit
     in
     let files_length = List.length files in
     let run test_fn =
       Lwt_list.iteri_s
         (fun i file ->
           let%lwt () =
             Templates.make_template
               file
               load_duration
               load_duration_unit
               load_arrivalrate
               load_arrivalrate_unit
           in
           let%lwt results_dir = test_fn file in
           let%lwt () = run_python_stats results_dir in
           if i + 1 <> files_length then Lwt_unix.sleep 30. else Lwt.return_unit )
         files
     in
     Lwt_list.iter_s
       (fun server ->
         match server with
         | Mirage -> run @@ test_docker mirage "" @@ server_to_string Mirage
         | Ejabberd ->
           run
           @@ test_docker
                ejabberd
                "$(pwd)/docker/ejabberd/ejabberd.yml:/home/ejabberd/conf/ejabberd.yml"
           @@ server_to_string Ejabberd
         | Tigase ->
           run
           @@ test_docker
                tigase
                "$(pwd)/docker/tigase/init.properties:/opt/tigase-server/etc/init.properties"
           @@ server_to_string Tigase
         | Prosody ->
           run
           @@ test_docker
                prosody
                "$(pwd)/docker/prosody/prosody.cfg.lua:/etc/prosody/prosody.cfg.lua"
           @@ server_to_string Prosody
         | None -> run @@ test_none @@ server_to_string None )
       servers)
;;

(* Command line parsing *)

let servers =
  let doc = "Run performance tests against the MirageOS unikernel" in
  let mirage = Mirage, Arg.info ["m"; "mirage"] ~doc in
  let doc = "Run performance tests against the Ejabberd server" in
  let ejabberd = Ejabberd, Arg.info ["e"; "ejabberd"] ~doc in
  let doc = "Run performance tests against the Tigase server" in
  let tigase = Tigase, Arg.info ["t"; "tigase"] ~doc in
  let doc = "Run performance tests against the Prosody server" in
  let prosody = Prosody, Arg.info ["p"; "prosody"] ~doc in
  let doc = "No automated server creation, just run tsung" in
  let none = None, Arg.info ["n"; "none"] ~doc in
  Arg.(value & vflag_all [] [mirage; ejabberd; tigase; prosody; none])
;;

let files =
  let doc = "The xml files to run tsung with." in
  Arg.(value & pos_all file [] & info [] ~doc)
;;

let load_duration =
  let doc = "The duration to run the tests for" in
  Arg.(value & opt int 1 & info ["ld"; "load-duration"] ~doc)
;;

let load_duration_unit =
  let doc = "The unit (minute or second) for the load duration." in
  Arg.(value & opt string "minute" & info ["ldu"; "load-duration-unit"] ~doc)
;;

let load_arrivalrate =
  let doc = "The arrivalrate of the users for the scenario." in
  Arg.(value & opt int 10 & info ["la"; "load-arrivalrate"] ~doc)
;;

let load_arrivalrate_unit =
  let doc = "The unit (minute or second) for the arrivalrate of users." in
  Arg.(value & opt string "second" & info ["lau"; "load-arrivalrate-unit"] ~doc)
;;

let cmd =
  let doc = "Run performance tests against a given target." in
  ( Term.(
      const performance
      $ servers
      $ files
      $ load_duration
      $ load_duration_unit
      $ load_arrivalrate
      $ load_arrivalrate_unit)
  , Term.info "performance" ~doc ~exits:Term.default_exits )
;;

let info =
  let doc = "Run the performance tests against a given target." in
  Term.info "performance" ~doc ~exits:Term.default_exits
;;

let () = Term.exit @@ Term.eval cmd
