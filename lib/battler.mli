open Chooser
open Rps

type winner = P1 | P2 | Tie
type result = { p1_log : choice list; p2_log : choice list; winner : winner }

val string_of_winner : winner -> string
val string_of_log : choice list -> string
val versus : p1:chooser -> p2:chooser -> winner
