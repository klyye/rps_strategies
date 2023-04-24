type choice = Rock | Paper | Scissors
type result = Win | Loss | Tie

let match_rps rock_outcome paper_outcome scissors_outcome choice =
  match choice with
  | Rock -> rock_outcome
  | Paper -> paper_outcome
  | Scissors -> scissors_outcome

let counter_to = match_rps Paper Scissors Rock
let beaten_by = match_rps Scissors Rock Paper
let damage = match_rps 1 1 1
let strategyRock _ = Rock
let strategyPaper _ = Paper

let make_choice strategy1 strategy2 log =
  let choice1 = strategy1 log in
  let choice2 = strategy2 log in
  (choice1, choice2)

(**
    returns the winner of two given strategies, Win iff strategy1 wins
*)
let rec winning_strategy ?(health1 = 100) ?(health2 = 100) ?(log = []) strategy1
    strategy2 =
  if health1 = 0 && health2 = 0 then Tie
  else if health1 <= 0 then Loss
  else if health2 <= 0 then Win
  else
    let choice1, choice2 = make_choice strategy1 strategy2 log in
    if choice1 = beaten_by choice2 then
      winning_strategy
        ~health1:(health1 - damage choice2)
        ~health2
        ~log:((choice1, choice2) :: log)
        strategy1 strategy2
    else if choice1 = counter_to choice2 then
      winning_strategy ~health1
        ~health2:(health2 - damage choice1)
        ~log:((choice1, choice2) :: log)
        strategy1 strategy2
    else
      (* both players take damage in event of a tie to prevent infinite recursion *)
      winning_strategy
        ~health1:(health1 - damage choice2)
        ~health2:(health2 - damage choice1)
        ~log:((choice1, choice2) :: log)
        strategy1 strategy2
