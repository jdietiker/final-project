open OUnit2
open Final_project
open Final_project.Gui
open Final_project.Gameboard

let init_tile_bag root =
  Array.to_list (Arg.read_arg (root ^ "/test/testbag.txt"))

let p1_tiles_i, bag1 = draw_tiles [] (init_tile_bag (project_root ()))
let p2_tiles_i, tiles_bag_i = draw_tiles [] bag1
let () = init_vars p1_tiles_i p2_tiles_i tiles_bag_i

let bag_tests =
  "tests for gui bag draw functions"
  >::: [
         ( "test draw tiles size" >:: fun _ ->
           assert_equal 7 (List.length p1_tiles_i) );
         ( "test draw tiles size 2" >:: fun _ ->
           assert_equal 3 (List.length p2_tiles_i) );
         ( "test draw tiles empty bag 2" >:: fun _ ->
           let _, bag1 = draw_tiles [] [] in
           let p2_tiles_i, _ = draw_tiles [] bag1 in
           assert_equal 0 (List.length p2_tiles_i) );
         ( "test draw tiles empty bag 1" >:: fun _ ->
           let p1_tiles_i, _ = draw_tiles [] [] in
           assert_equal 0 (List.length p1_tiles_i) );
         ( "test draw tiles limited bag" >:: fun _ ->
           let p1_tiles_i, _ = draw_tiles [] [ "A"; "B"; "C"; "D"; "E" ] in
           assert_equal 5 (List.length p1_tiles_i) );
         ( "test draw tiles limited bag" >:: fun _ ->
           let _, bag1 = draw_tiles [] [ "A"; "B"; "C"; "D"; "E" ] in
           let p2_tiles_i, _ = draw_tiles [] bag1 in
           assert_equal 0 (List.length p2_tiles_i) );
         ( "test draw tiles bag size 1" >:: fun _ ->
           let _, bag1 =
             draw_tiles [] [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "A" ]
           in
           let _, tiles_bag_i = draw_tiles [] bag1 in
           assert_equal 0 (List.length tiles_bag_i) );
         ( "test draw tiles bag size 2" >:: fun _ ->
           let _, bag1 =
             draw_tiles [] [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "A" ]
           in
           assert_equal 2 (List.length bag1) );
       ]

let board = init
let _ = set_letter 2 3 board "A"
let _ = set_letter 2 2 board "B"
let _ = set_letter 10 10 board "C"
let _ = set_letter 5 3 board "A"
let _ = play_letter 2 3 board
let _ = play_letter 2 2 board
let _ = play_letter 10 10 board
let _ = play_letter 5 3 board

let gameboard_tests =
  "tests for empty gameboard"
  >::: [
         ( "test multipliers" >:: fun _ ->
           assert_equal (multiplier_at board 0 0) TW );
         ( "test multipliers 2" >:: fun _ ->
           assert_equal (multiplier_at board 5 0) No );
         ( "test multipliers 3" >:: fun _ ->
           assert_equal (multiplier_at board 9 1) TL );
         ( "test multipliers 4" >:: fun _ ->
           assert_equal (multiplier_at board 7 7) Star );
         ( "test multipliers 5" >:: fun _ ->
           assert_equal (multiplier_at board 14 14) TW );
         ( "test multipliers 6" >:: fun _ ->
           assert_equal (multiplier_at board 4 10) DW );
         ("test blank" >:: fun _ -> assert_equal (was_blank_at board 5 2) false);
         ( "test blank 2" >:: fun _ ->
           assert_equal (was_blank_at board 4 10) false );
         ("test played" >:: fun _ -> assert_equal (played_at board 2 14) false);
         ("test played 2" >:: fun _ -> assert_equal (played_at board 7 7) false);
         ("test played 3" >:: fun _ -> assert_equal (played_at board 10 10) true);
         ("test letter_at" >:: fun _ -> assert_equal (letter_at board 0 0) "");
         ("test letter_at 2" >:: fun _ -> assert_equal (letter_at board 0 0) "");
         ("test letter_at 3" >:: fun _ -> assert_equal (letter_at board 2 2) "B");
         ( "test letter_at 4" >:: fun _ ->
           assert_equal (letter_at board 10 10) "C" );
         ( "test letter_at 5" >:: fun _ ->
           assert_equal (letter_at board 2 3) (letter_at board 5 3) );
       ]

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

let _ = run_test_tt_main bag_tests
let _ = run_test_tt_main gameboard_tests
let _ = run_test_tt_main scrabbl_tests
