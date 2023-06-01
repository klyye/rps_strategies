(* open Printf *)

type choice = Rock | Paper | Scissors
type result = P1 | P2 | Tie

type chooser =
  | Blind of choice
  | SeesOpponent of (choice list -> choice)
  | SeesSelf of (choice list -> choice)

type player = { chooser : chooser; log : choice list; health : int }

let default_health = 100
let make_player chooser = { chooser; log = []; health = default_health }

let string_of_result res =
  match res with P1 -> "P1" | P2 -> "P2" | Tie -> "Tie"

let match_rps rock_outcome paper_outcome scissors_outcome choice =
  match choice with
  | Rock -> rock_outcome
  | Paper -> paper_outcome
  | Scissors -> scissors_outcome

let string_of_choice = match_rps "Rock" "Paper" "Scissors"
let counter_to = match_rps Paper Scissors Rock
let beaten_by = match_rps Scissors Rock Paper
let damage = match_rps 10 10 10

let make_choice chooser self_log opponent_log =
  match chooser with
  | Blind f -> f
  | SeesOpponent f -> f opponent_log
  | SeesSelf f -> f self_log

(**
    returns the winner of two given players, Win iff player1 wins
*)
let rec do_battle player1 player2 =
  if player1.health = 0 && player2.health = 0 then Tie
  else if player1.health <= 0 then P2
  else if player2.health <= 0 then P1
  else
    let choice1 = make_choice player1.chooser player1.log player2.log in
    let choice2 = make_choice player2.chooser player2.log player1.log in
    let player1 = { player1 with log = choice1 :: player1.log } in
    let player2 = { player2 with log = choice2 :: player2.log } in
    if choice1 = beaten_by choice2 then
      do_battle
        { player1 with health = player1.health - damage choice2 }
        player2
    else if choice1 = counter_to choice2 then
      do_battle player1
        { player2 with health = player2.health - damage choice1 }
    else
      (* both players take damage in event of a tie to prevent infinite recursion *)
      do_battle
        { player1 with health = player1.health - damage choice2 }
        { player2 with health = player2.health - damage choice1 }

let versus ~p1 ~p2 = do_battle (make_player p1) (make_player p2)
