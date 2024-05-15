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
let _ = play_letter 9 10 board
let _ = play_letter 5 3 board

let gameboard_tests =
  "tests for setting and playing letters on gameboard"
  >::: [
         ("test new board is empty"
         >::
         let new_board = init () in
         fun _ -> assert_equal (is_empty new_board) true);
         ("test setting letter keeps empty board empty"
         >::
         let new_board = init () in
         let _ = set_letter 3 2 new_board "D" in
         fun _ -> assert_equal (is_empty new_board) true);
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
         ("test played 3" >:: fun _ -> assert_equal (played_at board 9 10) true);
         ("test letter_at" >:: fun _ -> assert_equal (letter_at board 0 0) "");
         ("test letter_at 2" >:: fun _ -> assert_equal (letter_at board 0 0) "");
         ("test letter_at 3" >:: fun _ -> assert_equal (letter_at board 2 2) "B");
         ( "test letter_at 4" >:: fun _ ->
           assert_equal (letter_at board 10 10) "C" );
         ("test letter_at 5" >:: fun _ -> assert_equal (letter_at board 5 3) "A");
         ("test letter_at 6" >:: fun _ -> assert_equal (letter_at board 2 3) "A");
         ( "test letter_at 6" >:: fun _ ->
           assert_equal (letter_at board 2 3 == letter_at board 5 3) true );
         ("test setting letter doesn't play it"
         >::
         let _ = set_letter 4 4 board "D" in
         fun _ -> assert_equal (played_at board 4 4) false);
         ("test setting overriding letters plays correct letter"
         >::
         let _ = set_letter 5 4 board "D" in
         let _ = set_letter 5 4 board "E" in
         let _ = play_letter 5 4 board in
         fun _ -> assert_equal (letter_at board 5 4) "E");
         ("test cannot override already played letter"
         >::
         let _ = set_letter 6 4 board "D" in
         let _ = play_letter 6 4 board in
         let _ = set_letter 6 4 board "E" in
         fun _ -> assert_equal (played_at board 6 4) true);
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

let gameboard_function_tests =
  "tests gameboard functions: "
  >::: [
         ( "test played_at 7 3" >:: fun _ ->
           assert_equal (played_at board 7 3) false );
         ( "test played_at 7 4" >:: fun _ ->
           assert_equal (played_at board 7 4) false );
         ( "test multiplier_at 7 7" >:: fun _ ->
           assert_equal (multiplier_at board 7 7) Star );
         ( "test multiplier_at 7 3" >:: fun _ ->
           assert_equal (multiplier_at board 7 3) DL );
         ( "test multiplier_at 7 0" >:: fun _ ->
           assert_equal (multiplier_at board 7 0) TW );
         ( "test multiplier_at 7 1" >:: fun _ ->
           assert_equal (multiplier_at board 7 1) No );
         ( "test was blank at 7 7" >:: fun _ ->
           assert_equal (was_blank_at board 7 7) false );
         ( "test was blank at 7 3" >:: fun _ ->
           assert_equal (was_blank_at board 7 3) false );
       ]

let board2 = init ()
let lst1 = [ ("H", 7, 3); ("E", 7, 4); ("L", 7, 5); ("L", 7, 6); ("O", 7, 7) ]
let lst2 = [ ("A", 8, 6); ("W", 9, 6) ]
let lst3 = [ ("N", 8, 7) ]
let lst4 = [ ("W", 6, 2); ("R", 6, 3); ("B", 6, 4) ]

let rec set_lst gb = function
  | [] -> ()
  | (letter, x, y) :: t ->
      let _ = set_letter x y gb letter in
      set_lst gb t

(** [play_lst gb lst] sets each item in [lst] to be played in [gb].*)
let rec play_lst gb = function
  | [] -> ()
  | (_, x, y) :: t ->
      let _ = play_letter x y gb in
      play_lst gb t

let eval_tests =
  "tests for evaluation of points"
  >::: [
         ("test played" >:: fun _ -> assert_equal (played_at board2 7 4) false);
         ("test letter_at" >:: fun _ -> assert_equal (letter_at board2 7 3) "H");
         ( "test letter_at 2" >:: fun _ ->
           assert_equal (letter_at board2 7 4) "E" );
         ("test letter_at 3" >:: fun _ -> assert_equal (letter_at board2 5 3) "");
         ( "test eval HELLO" >:: fun _ ->
           print_endline "running test for hello";
           assert_equal (Scrabbl.eval_guess board2 lst1) 24 );
         ( "test played 2" >:: fun _ ->
           play_lst board2 lst1;
           assert_equal (played_at board2 7 4) true );
         ( "test add word" >:: fun _ ->
           print_endline "running test for LOW";

           play_lst board2 lst1;
           set_lst board2 lst2;

           assert_equal (Scrabbl.eval_guess board2 lst2) 7 );
         ( "test double words" >:: fun _ ->
           print_endline "running test for double words";
           let prev_lst = lst1 @ lst2 in
           play_lst board2 prev_lst;
           set_lst board2 (lst3 @ prev_lst);
           assert_equal (Scrabbl.eval_guess board2 lst3) 4 );
       ]

let board3 = init ()

let rec print_array = function
  | [] -> ()
  | (str, _, _, _) :: t ->
      print_string (str ^ ",");
      print_array t

let get_all_words_test =
  "tests for get_all_words"
  >::: [
         ( "test get_all_words HELLO" >:: fun _ ->
           (* set_lst board3 lst1; *)
           assert_equal
             (Scrabbl.get_all_words board3 lst1)
             (Some [ ("HELLO", 7, 3, true) ]) );
         ( "test invalid cases" >:: fun _ ->
           set_lst board3 lst1;
           assert_equal (Scrabbl.get_all_words board lst1) None );
         ( "test invalid cases 2" >:: fun _ ->
           set_lst board3 lst1;
           assert_equal (Scrabbl.eval_guess board3 lst4) (-1) );
       ]

let _ = set_lst board2 lst1
let _ = set_lst board3 lst1
let _ = run_test_tt_main bag_tests
let _ = run_test_tt_main gameboard_tests
let _ = run_test_tt_main score_tests

(* let _ = run_test_tt_main eval_tests *)
let _ = run_test_tt_main gameboard_function_tests

(* let board = init () let _ = set_lst board guess_lst *)
let _ = run_test_tt_main get_all_words_test
