open Printf

type choice = Rock | Paper | Scissors
type result = Win | Loss | Tie

type player = {
  strategy : choice list -> choice list -> choice;
  name : string;
  log : choice list;
  health : int;
}

let string_of_result res =
  match res with Win -> "Win" | Loss -> "Loss" | Tie -> "Tie"

let match_rps rock_outcome paper_outcome scissors_outcome choice =
  match choice with
  | Rock -> rock_outcome
  | Paper -> paper_outcome
  | Scissors -> scissors_outcome

let string_of_rps = match_rps "Rock" "Paper" "Scissors"
let counter_to = match_rps Paper Scissors Rock
let beaten_by = match_rps Scissors Rock Paper
let damage = match_rps 1 1 1
let strategyRock _ _ = Rock
let strategyPaper _ _ = Paper
let strategyCounter log _ = match log with [ h ] -> counter_to h | _ -> Rock

let make_choice strategy1 strategy2 log1 log2 =
  let choice1 = strategy1 log1 log2 in
  let choice2 = strategy2 log2 log1 in
  (choice1, choice2)

(**
    returns the winner of two given players, Win iff player1 wins
*)
let rec do_battle ?(debug = false) player1 player2 =
  if player1.health = 0 && player2.health = 0 then Tie
  else if player1.health <= 0 then Loss
  else if player2.health <= 0 then Win
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
        { player1 with health = player1.health - damage choice2 }
        player2
    else if choice1 = counter_to choice2 then
      do_battle ~debug player1
        { player2 with health = player2.health - damage choice1 }
    else
      (* both players take damage in event of a tie to prevent infinite recursion *)
      do_battle ~debug
        { player1 with health = player1.health - damage choice2 }
        { player2 with health = player2.health - damage choice1 }
