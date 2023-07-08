open Rps
open Chooser

type winner = P1 | P2 | Tie
type result = { p1_log : choice list; p2_log : choice list; winner : winner }
type player = { chooser : chooser; log : choice list; health : int }

let default_health = 9
let make_player chooser = { chooser; log = []; health = default_health }
let damage = 1
let string_of_winner x = match x with P1 -> "P1" | P2 -> "P2" | Tie -> "Tie"

let rec string_of_log log =
  match log with
  | h :: rest -> string_of_choice h ^ ", " ^ string_of_log rest
  | [] -> ""

(**
    returns the winner of two given players, as well as a log of moves
*)
let rec do_battle p1 p2 =
  if p1.health <= 0 || p2.health <= 0 then
    if p1.health = p2.health then
      { p1_log = p1.log; p2_log = p2.log; winner = Tie }
    else if p1.health < p2.health then
      { p1_log = p1.log; p2_log = p2.log; winner = P2 }
    else { p1_log = p1.log; p2_log = p2.log; winner = P1 }
  else
    let choice1 = make_choice p1.chooser p1.log p2.log in
    let choice2 = make_choice p2.chooser p2.log p1.log in
    let p1 = { p1 with log = choice1 :: p1.log } in
    let p2 = { p2 with log = choice2 :: p2.log } in
    if choice1 = beaten_by choice2 then
      do_battle { p1 with health = p1.health - damage } p2
    else if choice1 = counter_to choice2 then
      do_battle p1 { p2 with health = p2.health - damage }
    else
      (* both players take damage in event of a tie to prevent infinite recursion *)
      do_battle
        { p1 with health = p1.health - damage }
        { p2 with health = p2.health - damage }

let versus ~p1 ~p2 = do_battle (make_player p1) (make_player p2)
