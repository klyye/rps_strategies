type t

val empty : t
val add_player : t -> string -> t
val to_list : t -> (string * int) list
val update_rating : t -> winning_player:string -> losing_player:string -> t
