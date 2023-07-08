let rec all_pairs lst =
  match lst with
  | h :: t -> List.map (fun e -> (h, e)) t @ all_pairs t
  | [] -> []

(** i miss java tostring *)
let string_of_int_pair pair =
  let f, s = pair in
  "(" ^ string_of_int f ^ "," ^ string_of_int s ^ ")"

let string_of_list to_string list = String.concat "" (List.map to_string list)
let swap f a1 a2 = f a2 a1
