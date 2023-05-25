module type mmr = sig
  val update_rating : winning_rating:int -> losing_rating:int -> int * int
end

module Incremental : mmr = struct
  let update_rating ~winning_rating ~losing_rating =
    (winning_rating - 1, losing_rating + 1)
end
