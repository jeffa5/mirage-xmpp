open Jingoo

let make_template
    file load_duration load_duration_unit load_arrivalrate load_arrivalrate_unit =
  let open Jg_types in
  let auth = Jg_template.from_file "test/performance/templates/auth.xml" in
  let selected_sessions = Jg_template.from_file file ~models:["auth", Tstr auth] in
  let sessions = "sessions", Tstr selected_sessions in
  let loglevel = "loglevel", Tstr "debug" in
  let backend = "backend", Tstr "fullstats" in
  let dumptraffic = "dumptraffic", Tstr "true" in
  let load_duration = "load_duration", Tint load_duration in
  let load_duration_unit = "load_duration_unit", Tstr load_duration_unit in
  let load_arrivalrate = "load_arrivalrate", Tint load_arrivalrate in
  let load_arrivalrate_unit = "load_arrivalrate_unit", Tstr load_arrivalrate_unit in
  let result =
    Jg_template.from_file
      "test/performance/templates/header.xml"
      ~models:
        [ loglevel
        ; backend
        ; dumptraffic
        ; load_duration
        ; load_duration_unit
        ; load_arrivalrate
        ; load_arrivalrate_unit
        ; sessions ]
  in
  let replace str by s = Re.replace_string (Re.compile (Re.str str)) ~by s in
  let unescape_html html =
    html
    |> replace "&#38;" "&"
    |> replace "&#34;" "\""
    |> replace "&#39;" "'"
    |> replace "&#60;" "<"
    |> replace "&#62;" ">"
  in
  let%lwt tsung_file = Lwt_io.open_file ~mode:Output "test/performance/tsung.xml" in
  let%lwt () = Lwt_io.fprint tsung_file (unescape_html result) in
  Lwt_io.close tsung_file
;;
