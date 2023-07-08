open OUnit2
open Rps_strategies

let battler_tests =
  let open Battler in
  let open Choosers in
  "test suite for battler"
  >::: [
         ( "paper beats rock" >:: fun _ ->
           assert_equal P1 (versus ~p1:chooserPaper ~p2:chooserRock).winner
             ~printer:string_of_winner );
         ( "rock loses to paper" >:: fun _ ->
           assert_equal P2 (versus ~p1:chooserRock ~p2:chooserPaper).winner
             ~printer:string_of_winner );
         ( "rock ties to rock" >:: fun _ ->
           assert_equal Tie (versus ~p1:chooserRock ~p2:chooserRock).winner
             ~printer:string_of_winner );
         ( "counterpick beats rock" >:: fun _ ->
           assert_equal P1 (versus ~p1:chooserCounter ~p2:chooserRock).winner
             ~printer:string_of_winner );
         ( "counterpick beats paper" >:: fun _ ->
           assert_equal P1 (versus ~p1:chooserCounter ~p2:chooserPaper).winner
             ~printer:string_of_winner );
         ( "counterpick beats scissors" >:: fun _ ->
           assert_equal P1
             (versus ~p1:chooserCounter ~p2:chooserScissors).winner
             ~printer:string_of_winner );
       ]

let rating_tests =
  let open Rating in
  let abc =
    empty |> add_player "Alice" |> add_player "Bob" |> add_player "Cody"
  in
  let a_beats_b = update_rating ~winning:"Alice" ~losing:"Bob" abc in
  "test suite for rating"
  >::: [
         ("empty test" >:: fun _ -> assert_equal None (rating "Oh boy" empty));
         ( "default rating" >:: fun _ ->
           assert_equal (Some default_rating) (rating "Alice" abc) );
         ( "alice beats bob" >:: fun _ ->
           assert_bool "Alice should have higher rating than Bob"
             (rating "Alice" a_beats_b > rating "Bob" a_beats_b) );
         ( "loser lower than default rating" >:: fun _ ->
           assert_bool "Bob should have lower than Cody"
             (rating "Cody" a_beats_b > rating "Bob" a_beats_b) );
       ]

let util_tests =
  let open Utility in
  "test suite for utils"
  >::: [
         ( "1234 list" >:: fun _ ->
           assert_equal
             [ (1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4) ]
             (all_pairs [ 1; 2; 3; 4 ]) (* world's jankiest tuple printer *)
             ~printer:(string_of_list string_of_int_pair) );
         ( "010 list" >:: fun _ ->
           assert_equal
             [ (0, 1); (0, 0); (1, 0) ]
             (all_pairs [ 0; 1; 0 ])
             ~printer:(string_of_list string_of_int_pair) );
         ( "empty list" >:: fun _ ->
           assert_equal [] (all_pairs [])
             ~printer:(string_of_list string_of_int_pair) );
       ]

let chooser_tests =
  let open Chooser in
  let open Choosers in
  let open Rps in
  "test suite for chooser"
  >::: [
         ( "rock chooser chooses rock" >:: fun _ ->
           assert_equal Rock (make_choice chooserRock [] []) );
         ( "counterpick picks rock against scissors" >:: fun _ ->
           assert_equal Rock (make_choice chooserCounter [] [ Scissors ]) );
         ( "counterpick multiple turns" >:: fun _ ->
           assert_equal Paper
             (make_choice chooserCounter [ Rock; Paper ] [ Rock; Scissors ]) );
       ]

let _ = run_test_tt_main battler_tests
let _ = run_test_tt_main util_tests
let _ = run_test_tt_main rating_tests
let _ = run_test_tt_main chooser_tests
