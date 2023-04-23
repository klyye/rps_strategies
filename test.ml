open OUnit2
open Rps

let tests =
  "test suite for rps"
  >::: [
         ( "placeholder" >:: fun _ ->
           assert_equal "placeholder1" (make_choice Rock "1") );
       ]

let _ = run_test_tt_main tests
