type t = (string * int) list

let empty = []
let default_rating = 1000
let add_player assoc_lst name = (name, default_rating) :: assoc_lst
let to_list x = x

let update_rating assoc_lst ~winning_player ~losing_player =
  (* bruh *)
  List.map
    (fun pair ->
      let name, rating = pair in
      ( name,
        if name = winning_player then rating + 1
        else if name = losing_player then rating - 1
        else rating ))
    assoc_lst
