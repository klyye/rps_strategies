open Rps_strategies

(* manually hardcode names for all choosers *)
let chooser_map =
  let open Choosers in
  [ ("All Rock", chooserRock); ("All Paper", chooserPaper) ]

let rating_table =
  let open Rating in
  List.fold_right add_player (List.map fst chooser_map) empty

let print_rating table name =
  print_string
    (name ^ ": "
    ^ (match Rating.get_rating name table with
      | Some i -> string_of_int i
      | None -> "None")
    ^ "\n")

let () =
  let _ = print_string "\n" in
  List.iter (print_rating rating_table) (Rating.get_names_list rating_table)
