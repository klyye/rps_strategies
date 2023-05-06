type result = Win | Loss | Tie

let update_rating rating1 rating2 result =
  match result with Win -> rating1 + 1 | _ -> rating2 + 1
