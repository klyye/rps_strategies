open Rps
open Chooser

let chooserRock = Blind Rock
let chooserPaper = Blind Paper
let chooserScissors = Blind Scissors

let chooserCounter =
  SeesOpponent
    (fun log -> match log with h :: _ -> counter_to h | [] -> Scissors)

let chooserCycle =
  SeesSelf (fun log -> match log with h :: _ -> counter_to h | [] -> Rock)

(* manually hardcode names for all choosers *)
let chooser_map =
  [
    ("All Rock", chooserRock);
    ("All Paper", chooserPaper);
    ("All Scissors", chooserScissors);
    ("Counterpick", chooserCounter);
    ("Cycle", chooserCycle);
  ]

let chooser_list = List.map snd chooser_map
let names_list = List.map fst chooser_map
let name_to_chooser name = List.assoc name chooser_map
