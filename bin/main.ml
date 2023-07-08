open Rps_strategies

let rating_table =
  let open Rating in
  List.fold_right add_player Choosers.names_list empty

let print_rating table name =
  print_string
    (name ^ ": "
    ^ (match Rating.rating name table with
      | Some i -> string_of_int i
      | None -> "None")
    ^ "\n")

(* plays matchup and updates table *)
let play_matchup table pair =
  let name1, name2 = pair in
  let p1 = Choosers.name_to_chooser name1 in
  let p2 = Choosers.name_to_chooser name2 in
  match (Battler.versus ~p1 ~p2).result with
  | P1 -> Rating.update_rating ~winning:name1 ~losing:name2 table
  | P2 -> Rating.update_rating ~winning:name2 ~losing:name1 table
  | Tie -> table

(* generates all matchups *)
let play_matchups table =
  let matchups = Utility.all_pairs (Rating.names_list table) in
  List.fold_left play_matchup table matchups

let () =
  let table = play_matchups rating_table in
  let _ = print_string "\n" in
  List.iter (print_rating table) (Rating.names_list table)

(** TODO: make them all play against each other, give them ratings *)
