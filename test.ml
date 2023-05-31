open OUnit2

let rps_tests =
  let open Rps in
  let chooserRock = Blind Rock in
  let chooserPaper = Blind Paper in
  let chooserCounter =
    SeesOpponent
      (fun log -> match log with h :: _ -> counter_to h | [] -> Rock)
  in
  "test suite for rps"
  >::: [
         ( "paper beats rock" >:: fun _ ->
           assert_equal P1
             (versus ~p1:chooserPaper ~p2:chooserRock)
             ~printer:string_of_result );
         ( "rock loses to paper" >:: fun _ ->
           assert_equal P2
             (versus ~p1:chooserRock ~p2:chooserPaper)
             ~printer:string_of_result );
         ( "rock ties to rock" >:: fun _ ->
           assert_equal Tie
             (versus ~p1:chooserRock ~p2:chooserRock)
             ~printer:string_of_result );
         ( "counterpick beats rock" >:: fun _ ->
           assert_equal P1
             (versus ~p1:chooserCounter ~p2:chooserRock)
             ~printer:string_of_result );
       ]

(** i miss java tostring *)
let string_of_int_pair pair =
  let f, s = pair in
  "(" ^ string_of_int f ^ "," ^ string_of_int s ^ ")"

let util_tests =
  let open Utility in
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
