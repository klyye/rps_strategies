open Printf

let answers =
  [
    "cornucopia";
    "alligator";
    "cottage";
    "inconspicuous";
    "specialized";
    "alchemist";
    "tasteful";
    "evocative";
    "cinematic";
    "guitar";
    "palisade";
    "manipulate";
    "instrumental";
    "intoxicating";
    "poetic";
    "nondescript";
  ]

let random_index lst =
  let i = Random.int (List.length lst) in
  List.nth lst i

let word_to_underscores word = String.map (fun _ -> '_') word

(**
  replaces underscores corresponding to the [guess] letter in [current_guessed] 
    *)
let guess_letter guess current_guessed answer =
  String.mapi
    (fun i answer_letter ->
      if answer_letter = guess then guess else current_guessed.[i])
    answer

let rec game_loop current_guessed answer =
  printf "%s\n" current_guessed;
  if current_guessed = answer then printf "Congrats, you guessed the answer!\n"
  else
    let next_letter = read_line () in
    if next_letter = "quit" then ()
    else if String.length next_letter != 1 then (
      printf "Enter one letter only.\n";
      (game_loop [@tailcall]) current_guessed answer)
    else
      let new_guess = guess_letter next_letter.[0] current_guessed answer in
      (game_loop [@tailcall]) new_guess answer

let main () =
  printf "\nType \"quit\" to quit.\n";
  Random.self_init ();
  let answer = random_index answers in
  game_loop (word_to_underscores answer) answer
;;

main ()
