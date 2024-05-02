open OUnit2
open Final_project

let scrabbl_tests =
  "tests for is_empty"
  >::: [
         ( "test dictionary membership 1" >:: fun _ ->
           assert_equal true (Scrabbl.valid_word "hello") );
         ( "test dictionary membership 2" >:: fun _ ->
           assert_equal true (Scrabbl.valid_word "zymolytic") );
         ( "test dictionary membership 3" >:: fun _ ->
           assert_equal false (Scrabbl.valid_word "uncr") );
         ( "test dictionary membership 4" >:: fun _ ->
           assert_equal false (Scrabbl.valid_word "zbungmboc") );
       ]

let _ = run_test_tt_main scrabbl_tests
