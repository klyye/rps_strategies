type choice = Rock | Paper | Scissors
type result = P1 | P2 | Tie
type rps_game = { p1_log : choice list; p2_log : choice list; result : result }

type chooser =
  | Blind of choice
  | SeesOpponent of (choice list -> choice)
  | SeesSelf of (choice list -> choice)

val versus : p1:chooser -> p2:chooser -> rps_game
val counter_to : choice -> choice
val beaten_by : choice -> choice
val string_of_result : result -> string
val string_of_choice : choice -> string
