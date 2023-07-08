type choice = Rock | Paper | Scissors
type result = P1 | P2 | Tie
type rps_game = { p1_log : choice list; p2_log : choice list; result : result }

type chooser =
  | Blind of choice
  | SeesOpponent of (choice list -> choice)
  | SeesSelf of (choice list -> choice)

type player = { chooser : chooser; log : choice list; health : int }

let default_health = 9999
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
    returns the winner of two given players, as well as a log of moves
*)
let rec do_battle p1 p2 =
  if p1.health = 0 && p2.health = 0 then
    { p1_log = p1.log; p2_log = p2.log; result = Tie }
  else if p1.health <= 0 then { p1_log = p1.log; p2_log = p2.log; result = P2 }
  else if p2.health <= 0 then { p1_log = p1.log; p2_log = p2.log; result = P1 }
  else
    let choice1 = make_choice p1.chooser p1.log p2.log in
    let choice2 = make_choice p2.chooser p2.log p1.log in
    let p1 = { p1 with log = choice1 :: p1.log } in
    let p2 = { p2 with log = choice2 :: p2.log } in
    if choice1 = beaten_by choice2 then
      do_battle { p1 with health = p1.health - damage choice2 } p2
    else if choice1 = counter_to choice2 then
      do_battle p1 { p2 with health = p2.health - damage choice1 }
    else
      (* both players take damage in event of a tie to prevent infinite recursion *)
      do_battle
        { p1 with health = p1.health - damage choice2 }
        { p2 with health = p2.health - damage choice1 }

let versus ~p1 ~p2 = do_battle (make_player p1) (make_player p2)
