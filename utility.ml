let rec all_pairs lst =
  match lst with
  | h :: t -> List.map (fun e -> (h, e)) t @ all_pairs t
  | [] -> []

let string_of_list to_string list = String.concat "" (List.map to_string list)
(* let play_matchup player1 player2 = ""

   (** TODO function that pits every strategy against every other strategy N^2 matches
       returns a list of players with their new ratings after playing every possible matchup *)
   let rec play_all_combinations players matchups =
     match matchups with
     | h :: t -> List.map (fun player -> player) players
     | [] -> players *)
