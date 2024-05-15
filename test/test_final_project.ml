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

let board = init ()
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
         ("test blank 3"
         >::
         let _ = set_was_blank 9 9 board true in
         fun _ -> assert_equal (was_blank_at board 9 9) true);
         ("test played" >:: fun _ -> assert_equal (played_at board 2 14) false);
         ("test played 2" >:: fun _ -> assert_equal (played_at board 7 7) false);
         ("test played 3" >:: fun _ -> assert_equal (played_at board 10 10) true);
         ("test letter_at" >:: fun _ -> assert_equal (letter_at board 0 0) "");
         ("test letter_at 2" >:: fun _ -> assert_equal (letter_at board 0 0) "");
         ("test letter_at 3" >:: fun _ -> assert_equal (letter_at board 2 2) "B");
         ( "test letter_at 4" >:: fun _ ->
           assert_equal (letter_at board 10 10) "C" );
         ("test letter_at 5" >:: fun _ -> assert_equal (letter_at board 5 3) "A");
         ("test letter_at 6" >:: fun _ -> assert_equal (letter_at board 2 3) "A");
         ( "test letter_at 6" >:: fun _ ->
           assert_equal (letter_at board 2 3 == letter_at board 5 3) true );
       ]

let score_tests =
  "tests for dictionary and letters"
  >::: [
         ( "test dictionary membership 1" >:: fun _ ->
           assert_equal true (Scrabbl.valid_word "hello") );
         ( "test dictionary membership 2" >:: fun _ ->
           assert_equal true (Scrabbl.valid_word "zymolytic") );
         ( "test dictionary membership 3" >:: fun _ ->
           assert_equal false (Scrabbl.valid_word "uncr") );
         ( "test dictionary membership 4" >:: fun _ ->
           assert_equal false (Scrabbl.valid_word "zbungmboc") );
         ( "verify score for invalid letters" >:: fun _ ->
           assert_equal 0 (Scrabbl.point_val "a") );
         ( "verify score for invalid letters 2" >:: fun _ ->
           assert_equal 0 (Scrabbl.point_val "]") );
         ( "verify score for invalid letters 3" >:: fun _ ->
           assert_equal 0 (Scrabbl.point_val "ahsdf") );
         ( "verify score for invalid letters 4" >:: fun _ ->
           assert_equal 0 (Scrabbl.point_val "5") );
         ( "verify score for valid letters" >:: fun _ ->
           assert_equal 1 (Scrabbl.point_val "A") );
         ( "verify score for valid letters 2" >:: fun _ ->
           assert_equal 10 (Scrabbl.point_val "Z") );
         ( "verify score for valid letters 3" >:: fun _ ->
           assert_equal 8 (Scrabbl.point_val "X") );
         ( "verify score for valid letters 4" >:: fun _ ->
           assert_equal 3 (Scrabbl.point_val "M") );
       ]

let board = init ()

let guess_lst =
  [ ("H", 7, 3); ("E", 7, 4); ("L", 7, 5); ("L", 7, 6); ("O", 7, 7) ]

let rec set_lst gb = function
  | [] -> ()
  | (letter, x, y) :: t ->
      let _ = set_letter x y gb letter in
      set_lst gb t

let eval_tests =
  "tests for evaluation of points"
  >::: [
         ("test letter_at" >:: fun _ -> assert_equal (letter_at board 7 3) "H");
         ("test letter_at 2" >:: fun _ -> assert_equal (letter_at board 7 4) "E");
         ( "test eval HELLO" >:: fun _ ->
           let _ =
             print_endline (string_of_int (Scrabbl.eval_guess board guess_lst))
           in
           assert_equal (Scrabbl.eval_guess board guess_lst) 24 );
       ]

let _ = set_lst board guess_lst
let _ = run_test_tt_main bag_tests
let _ = run_test_tt_main gameboard_tests
let _ = run_test_tt_main score_tests
let _ = run_test_tt_main eval_tests
