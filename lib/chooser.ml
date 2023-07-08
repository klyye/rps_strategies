open Rps

type chooser =
  | Blind of choice
  | SeesOpponent of (choice list -> choice)
  | SeesSelf of (choice list -> choice)

let make_choice chooser self_log opponent_log =
  match chooser with
  | Blind f -> f
  | SeesOpponent f -> f opponent_log
  | SeesSelf f -> f self_log
