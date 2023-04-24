open OUnit2
open Rps

let tests =
  "test suite for rps"
  >::: [
         ( "paper beats rock" >:: fun _ ->
           assert_equal Win (winning_strategy strategyPaper strategyRock) );
         ( "rock loses to paper" >:: fun _ ->
           assert_equal Loss (winning_strategy strategyRock strategyPaper) );
         ( "rock ties to rock" >:: fun _ ->
           assert_equal Tie (winning_strategy strategyRock strategyRock) );
       ]

let _ = run_test_tt_main tests
