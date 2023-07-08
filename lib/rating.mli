(* a string (name) to int (rating) map *)
type t

val empty : t
val default_rating : int
val add_player : string -> t -> t
val to_list : t -> (string * int) list
val update_rating : winning:string -> losing:string -> t -> t
val rating : string -> t -> int option
val names_list : t -> string list
