open OUnit2
open Final_project
open Final_project.Gui
open Final_project.Gameboard

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

let init_tile_bag root = Array.to_list (Arg.read_arg (root ^ "/data/bag.txt"))
let p1_tiles_i, bag1 = draw_tiles [] (init_tile_bag (project_root ()))
let p2_tiles_i, tiles_bag_i = draw_tiles [] bag1
let () = init_vars p1_tiles_i p2_tiles_i tiles_bag_i
let _ = run_test_tt_main scrabbl_tests
