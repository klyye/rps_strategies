type choice = Rock | Paper | Scissors

let make_choice mine yours =
  match mine with
  | Rock -> "placeholder" ^ yours
  | Paper -> "placeholder"
  | Scissors -> "placeholder"
