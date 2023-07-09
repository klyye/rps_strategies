type choice = Rock | Paper | Scissors

let match_rps rock_outcome paper_outcome scissors_outcome choice =
  match choice with
  | Rock -> rock_outcome
  | Paper -> paper_outcome
  | Scissors -> scissors_outcome

let string_of_choice = match_rps "Rock" "Paper" "Scissors"
let counter_to = match_rps Paper Scissors Rock
let beaten_by = match_rps Scissors Rock Paper
