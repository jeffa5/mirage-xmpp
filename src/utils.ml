let mask_id s =
  match Astring.String.find_sub ~sub:"id='" s with
  | Some i ->
    (match Astring.String.find_sub ~start:(i + 4) ~sub:"'" s with
    | Some j ->
      Astring.String.with_index_range ~first:0 ~last:(i + 3) s
      ^ "<redacted_for_testing>"
      ^ Astring.String.with_index_range ~first:j s
    | None -> assert false)
  | None -> s
;;
