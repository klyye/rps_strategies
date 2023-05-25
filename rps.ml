open Printf

type choice = Rock | Paper | Scissors
type result = P1 | P2 | Tie

type player = {
  strategy : choice list -> choice list -> choice;
  name : string;
  log : choice list;
  health : int;
  rating : int;
}

let string_of_result res =
  match res with P1 -> "P1" | P2 -> "P2" | Tie -> "Tie"

let match_rps rock_outcome paper_outcome scissors_outcome choice =
  match choice with
  | Rock -> rock_outcome
  | Paper -> paper_outcome
  | Scissors -> scissors_outcome

let string_of_rps = match_rps "Rock" "Paper" "Scissors"
let string_of_list to_string list = String.concat "" (List.map to_string list)
let string_of_log = string_of_list string_of_rps
let counter_to = match_rps Paper Scissors Rock
let beaten_by = match_rps Scissors Rock Paper
let damage = match_rps 10 10 10
let strategyRock _ _ = Rock
let strategyPaper _ _ = Paper
let strategyCounter _ log = match log with h :: _ -> counter_to h | [] -> Rock

let make_choice strategy1 strategy2 log1 log2 =
  let choice1 = strategy1 log1 log2 in
  let choice2 = strategy2 log2 log1 in
  (choice1, choice2)

(**
    returns the winner of two given players, Win iff player1 wins
*)
let rec do_battle ?(debug = false) player1 player2 =
  if player1.health = 0 && player2.health = 0 then Tie
  else if player1.health <= 0 then P2
  else if player2.health <= 0 then P1
  else
    let choice1, choice2 =
      make_choice player1.strategy player2.strategy player1.log player2.log
    in
    let _ =
      if debug then
        printf "%s picks: %s, %s picks: %s\n" player1.name
          (string_of_rps choice1) player2.name (string_of_rps choice2)
      else ()
    in
    if choice1 = beaten_by choice2 then
      do_battle ~debug
        {
          player1 with
          health = player1.health - damage choice2;
          log = choice1 :: player1.log;
        }
        { player2 with log = choice2 :: player2.log }
    else if choice1 = counter_to choice2 then
      do_battle ~debug
        { player1 with log = choice1 :: player1.log }
        { player2 with health = player2.health - damage choice1 }
    else
      (* both players take damage in event of a tie to prevent infinite recursion *)
      do_battle ~debug
        {
          player1 with
          health = player1.health - damage choice2;
          log = choice1 :: player1.log;
        }
        {
          player2 with
          health = player2.health - damage choice1;
          log = choice2 :: player2.log;
        }

let rec all_pairs lst =
  match lst with
  | h :: t -> List.map (fun e -> (h, e)) t @ all_pairs t
  | [] -> []

(* let play_matchup player1 player2 = ""

   (** TODO function that pits every strategy against every other strategy N^2 matches
       returns a list of players with their new ratings after playing every possible matchup *)
   let rec play_all_combinations players matchups =
     match matchups with
     | h :: t -> List.map (fun player -> player) players
     | [] -> players *)
