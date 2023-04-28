open OUnit2
open Rps

let make_player strategy name = { strategy; name; log = []; health = 100 }
let playerPaper = make_player strategyPaper "paper only"
let playerRock = make_player strategyRock "rock only"
let playerCounter = make_player strategyCounter "counterpick"

let tests =
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

let _ = run_test_tt_main tests
