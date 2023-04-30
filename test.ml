open OUnit2
open Rps

let make_player strategy name =
  { strategy; name; log = []; health = 100; rating = 0 }

let playerPaper = make_player strategyPaper "paper only"
let playerRock = make_player strategyRock "rock only"
let playerCounter = make_player strategyCounter "counterpick"

let rps_tests =
  "test suite for rps"
  >::: [
         ( "paper beats rock" >:: fun _ ->
           assert_equal Win
             (do_battle playerPaper playerRock)
             ~printer:string_of_result );
         ( "rock loses to paper" >:: fun _ ->
           assert_equal Loss
             (do_battle playerRock playerPaper)
             ~printer:string_of_result );
         ( "rock ties to rock" >:: fun _ ->
           assert_equal Tie
             (do_battle playerRock playerRock)
             ~printer:string_of_result );
         ( "counterpick beats rock" >:: fun _ ->
           assert_equal Win
             (do_battle playerCounter playerRock)
             ~printer:string_of_result );
       ]

(** i miss java tostring *)
let string_of_int_pair pair =
  let f, s = pair in
  "(" ^ string_of_int f ^ "," ^ string_of_int s ^ ")"

let util_tests =
  "test suite for utils"
  >::: [
         ( "1234 list" >:: fun _ ->
           assert_equal
             [ (1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4) ]
             (all_pairs [ 1; 2; 3; 4 ]) (* world's jankiest tuple printer *)
             ~printer:(string_of_list string_of_int_pair) );
         ( "empty list" >:: fun _ ->
           assert_equal [] (all_pairs [])
             ~printer:(string_of_list string_of_int_pair) );
       ]

let _ = run_test_tt_main rps_tests
let _ = run_test_tt_main util_tests
