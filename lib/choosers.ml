open Rps

let chooserRock = Blind Rock
let chooserPaper = Blind Paper
let chooserScissors = Blind Scissors

let chooserCounter =
  SeesOpponent (fun log -> match log with h :: _ -> counter_to h | [] -> Rock)
