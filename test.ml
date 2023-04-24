open OUnit2
open Rps

let standard_battle = winning_strategy 100 100 []

let tests =
  "test suite for rps"
  >::: [
         ( "paper beats rock" >:: fun _ ->
           assert_equal Win (standard_battle strategyPaper strategyRock) );
         ( "rock loses to paper" >:: fun _ ->
           assert_equal Loss (standard_battle strategyRock strategyPaper) );
         ( "rock ties to rock" >:: fun _ ->
           assert_equal Tie (standard_battle strategyRock strategyRock) );
       ]

let _ = run_test_tt_main tests
