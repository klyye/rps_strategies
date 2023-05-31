type t

val empty : t
val default_rating : int
val add_player : string -> t -> t
val to_list : t -> (string * int) list
val update_rating : winning_player:string -> losing_player:string -> t -> t
val get_rating : string -> t -> int option
