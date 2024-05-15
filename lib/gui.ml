open Graphics
open Gameboard
open Scrabbl

let () = Random.self_init ()
let info_section_size = 120
let menu_bar_size = 20
let menu_button_width = 42
let grid_size = 601

let alphabet =
  Array.to_list (Arg.read_arg (project_root () ^ "/data/alphabet.txt"))

let board = init
let help = Arg.read_arg (project_root () ^ "/data/help.txt")

module StringSet = Set.Make (struct
  type t = string

  let compare a b =
    compare (String.lowercase_ascii a) (String.lowercase_ascii b)
end)

let help_lst = Array.to_list help

(** ex: [generate_coord 15 (600/15)] =
    [[0;40;80;120;160;200;240;280;320;360;400;440;480;520;560;600]] *)
let rec generate_coord num int =
  if num = 0 then [ 0 ] else generate_coord (num - 1) int @ ((num * int) :: [])

let coordinates = generate_coord 15 (grid_size / 15)

let get_tile_index x y =
  let tile_index = ref (-1) in
  if y >= 10 && y <= 10 + (grid_size / 15) then (
    let min_x = 116 in
    let cell_size = 40 in
    let gap_size = 10 in
    for i = 0 to 6 do
      let start_x = min_x + (i * gap_size) + (i * cell_size) in
      if x >= start_x && x <= start_x + cell_size then tile_index := i
    done;
    !tile_index)
  else !tile_index

let rec print_tiles_list lst x y cell_size gap_size selected_indices =
  match lst with
  | [] -> 0
  | h :: t ->
      let c = Graphics.rgb 237 210 153 in
      let my_yellow = Graphics.rgb 230 189 43 in
      if
        List.mem
          (get_tile_index (x + (cell_size / 2)) (y + (cell_size / 2)))
          selected_indices
      then set_color my_yellow
      else set_color c;
      fill_rect x y cell_size cell_size;
      set_color black;
      draw_rect x y cell_size cell_size;

      let a, b = text_size h in
      let x1 = x + (cell_size / 2) - (a / 2) + 1 in
      let y1 = y + ((cell_size / 2) - (b / 2)) + 2 in
      moveto x1 y1;
      set_text_size 69;
      if h <> "-" then draw_string h;

      let points =
        if point_val h = 0 then "" else string_of_int (point_val h)
      in
      let hor_move = a + 1 in
      let vert_move = b - 3 in
      moveto (x1 + hor_move) (y1 - vert_move);
      set_color (Graphics.rgb 151 151 151);
      draw_string points;
      print_tiles_list t
        (x + cell_size + gap_size)
        y cell_size gap_size selected_indices

(** prints the information about the current state of the game in the info
    section. *)
let print_info p1_points p2_points player1 curr_tiles tiles_bag =
  set_color white;
  let gap = 10 in
  let ts = 10 in
  let gray = Graphics.rgb 100 100 100 in
  fill_rect 0 0 (grid_size - (gap / 2)) (info_section_size - (gap / 2));
  moveto gap (info_section_size - (2 * gap));
  set_color black;
  set_text_size 80;
  draw_string
    ("Points:           Player 1: " ^ string_of_int p1_points ^ "    Player 2: "
   ^ string_of_int p2_points ^ "         Tiles left in bag: "
    ^ string_of_int (List.length tiles_bag));

  let y = ref (info_section_size - ((2 * gap) + ts)) in
  set_color gray;
  Graphics.moveto gap !y;
  Graphics.lineto (grid_size - gap) !y;
  moveto gap (!y - (2 * gap));
  set_color black;
  let s = if player1 then "Player 1" else "Player 2" in
  draw_string ("Current player:   " ^ s);

  y := !y - ((2 * gap) + ts);
  set_color gray;
  Graphics.moveto gap !y;
  Graphics.lineto (grid_size - gap) !y;

  let cs = grid_size / 15 in
  let _ = print_tiles_list curr_tiles 116 (!y - ts - cs) cs gap [] in

  moveto gap (!y - (2 * gap));
  set_color black;
  draw_string "Current tiles: "

(* paints the switching turn visual at the bottom *)
let paint_switch_turns player1 =
  let s1 = "SWITCHING TURNS" in
  let s2 =
    if player1 then "Player 1 --> Player 2" else "Player 2 --> Player 1"
  in
  let s1_len, height = text_size s1 in
  let s2_len, _ = text_size s2 in
  set_color white;
  fill_rect 0 0 grid_size info_section_size;

  set_color black;
  let x1 = (grid_size / 2) - (s1_len / 2) in
  let y1 = (info_section_size / 2) + 3 in
  let x2 = (grid_size / 2) - (s2_len / 2) in
  let y2 = (info_section_size / 2) - 3 - height in
  moveto x1 y1;
  draw_string s1;
  moveto x2 y2;
  draw_string s2

(* paints the x for the menu bar *)
let paint_x () =
  let x_color = Graphics.rgb 198 109 109 in
  Graphics.set_color x_color;
  Graphics.fill_rect
    (grid_size - menu_bar_size)
    (grid_size + info_section_size)
    menu_bar_size menu_bar_size;
  Graphics.set_color black;
  Graphics.draw_rect
    (grid_size - menu_bar_size)
    (grid_size + info_section_size)
    menu_bar_size menu_bar_size;
  (* draw x: *)
  let gap = 3 in
  Graphics.moveto
    (grid_size - menu_bar_size + gap)
    (grid_size + info_section_size + gap);

  Graphics.lineto (grid_size - gap)
    (grid_size + info_section_size + menu_bar_size - gap);
  Graphics.moveto
    (grid_size - menu_bar_size + gap)
    (grid_size + info_section_size + menu_bar_size - gap);
  Graphics.lineto (grid_size - gap) (grid_size + info_section_size + gap)

(* paint the help instructions: *)
let paint_instructions () =
  let menu_length = grid_size + info_section_size in
  Graphics.set_color white;
  Graphics.fill_rect 0
    (grid_size + (info_section_size - menu_length))
    grid_size menu_length;
  Graphics.set_color black;

  let gap = 5 in
  let line_gap = 2 in
  let _, str_length = text_size "hi" in
  let y = ref (grid_size + info_section_size - gap - str_length) in
  let length = List.length help_lst in
  for i = 0 to length - 1 do
    let s = List.nth help_lst i in
    Graphics.moveto gap !y;
    Graphics.draw_string s;
    y := !y - (line_gap + str_length)
  done

(* reset the background behind the menu to black *)
let paint_close_menu () =
  Graphics.set_color black;
  Graphics.fill_rect 0 (info_section_size - 1) grid_size (grid_size + 1)

(* paints the help button *)
let paint_help menu_color gap =
  Graphics.set_color menu_color;
  Graphics.fill_rect 0
    (grid_size + info_section_size)
    menu_button_width menu_bar_size;
  Graphics.set_color black;
  Graphics.draw_rect 0
    (grid_size + info_section_size)
    menu_button_width menu_bar_size;
  Graphics.moveto gap (grid_size + info_section_size + gap);
  Graphics.set_color black;
  Graphics.draw_string "Help"

let paint_pass menu_color gap =
  Graphics.set_color menu_color;
  Graphics.fill_rect (menu_button_width + 2)
    (grid_size + info_section_size)
    menu_button_width menu_bar_size;
  Graphics.set_color black;
  Graphics.draw_rect (menu_button_width + 1)
    (grid_size + info_section_size)
    menu_button_width menu_bar_size;
  Graphics.moveto
    (menu_button_width + 1 + gap)
    (grid_size + info_section_size + gap);
  Graphics.set_color black;
  Graphics.draw_string "Pass"

let paint_swap menu_color gap =
  Graphics.set_color menu_color;
  Graphics.fill_rect
    ((menu_button_width * 2) + 3)
    (grid_size + info_section_size)
    menu_button_width menu_bar_size;
  Graphics.set_color black;
  Graphics.draw_rect
    ((menu_button_width * 2) + 2)
    (grid_size + info_section_size)
    menu_button_width menu_bar_size;
  Graphics.moveto
    ((menu_button_width * 2) + 2 + gap)
    (grid_size + info_section_size + gap);
  Graphics.set_color black;
  Graphics.draw_string "Swap"

(** draws the menu button at the top *)
let print_menu_bar active =
  let menu_color = Graphics.rgb 182 182 182 in
  let gap = 5 in
  paint_help menu_color gap;
  paint_pass menu_color gap;
  paint_swap menu_color gap;

  if active then (
    paint_x ();
    paint_instructions ())

let rec draw_grid_h_aux = function
  | [] -> ()
  | h :: t ->
      let a, b = Graphics.current_point () in
      Graphics.moveto 0 (h + info_section_size);
      Graphics.lineto grid_size (h + info_section_size);
      draw_grid_h_aux t;
      Graphics.moveto a b

let rec draw_grid_v_aux = function
  | [] -> ()
  | h :: t ->
      let a, b = Graphics.current_point () in
      Graphics.moveto h info_section_size;
      Graphics.lineto h (grid_size + info_section_size);
      draw_grid_v_aux t;
      Graphics.moveto a b

let draw_grid =
  (* the size of the section which prints information about the player states *)
  Graphics.open_graph
    (" " ^ string_of_int grid_size ^ "x"
    ^ string_of_int (grid_size + info_section_size + menu_bar_size));
  set_window_title "SCRABBLE!";
  set_color black;
  set_text_size 200;
  draw_grid_h_aux coordinates;
  draw_grid_v_aux coordinates

(** prints the ith cell in the jth column of board. *)
let print_cell i j s =
  let a, b = text_size s in
  let vert_move = a + 1 in
  let hor_move = b - 3 in
  let points =
    if point_val s = 0 then ""
    else if was_blank_at board i j then ""
    else string_of_int (point_val s)
  in
  moveto
    ((grid_size / 15 * i) + (grid_size / 30) - (a / 2) + 1)
    (grid_size
    - ((grid_size / 15 * j) + (grid_size / 30) + (b / 2) + 2)
    + info_section_size);
  if was_blank_at board i j then set_color blue else set_color black;
  set_text_size 60;
  if s <> "-" then draw_string s;

  moveto
    ((grid_size / 15 * i) + (grid_size / 30) - (a / 2) + 1 + vert_move)
    (grid_size
    - ((grid_size / 15 * j) + (grid_size / 30) + (b / 2) + 2)
    - hor_move + info_section_size);
  set_color (Graphics.rgb 151 151 151);
  draw_string points

let print_el i j el =
  let s =
    if el_letter el = "" then
      match el_multiplier el with
      | No -> ""
      | TW -> "TW"
      | DW -> "DW"
      | TL -> "TL"
      | DL -> "DL"
      | Star -> "*"
    else el_letter el
  in
  print_cell i j s

(* custom colors: *)
let c_empty = Graphics.rgb 217 212 198
let c_tw = Graphics.rgb 244 93 111
let c_dw = Graphics.rgb 239 163 176
let c_tl = Graphics.rgb 17 125 178
let c_dl = Graphics.rgb 148 209 231
let c_star = Graphics.rgb 228 137 228
let c_played = Graphics.rgb 237 210 153
let c_unplayed = Graphics.rgb 255 243 216

let tile_color (el : Gameboard.elt) =
  if el_played el = true then c_played else c_unplayed

let print_cell_color i j (el : Gameboard.elt) =
  let color =
    match el_multiplier el with
    | No -> if el_letter el = "" then c_empty else tile_color el
    | TW -> if el_letter el = "" then c_tw else tile_color el
    | DW -> if el_letter el = "" then c_dw else tile_color el
    | TL -> if el_letter el = "" then c_tl else tile_color el
    | DL -> if el_letter el = "" then c_dl else tile_color el
    | Star -> if el_letter el = "" then c_star else tile_color el
  in
  set_color color;
  fill_rect
    ((grid_size / 15 * i) + 1)
    (grid_size - ((grid_size / 15 * j) + (grid_size / 15)) + info_section_size)
    ((grid_size / 15) - 2)
    ((grid_size / 15) - 2);
  print_el i j el;
  draw_grid

(** prints a board with colors according to the type of multiplier and inputted
    characters*)
let print_board board =
  for i = 0 to Gameboard.length board - 1 do
    for j = 0 to Gameboard.length board - 1 do
      print_cell_color i j (el_at board i j)
    done
  done

let paint_outline (row, col) =
  set_color red;
  draw_rect
    ((grid_size / 15 * row) + 1)
    (grid_size - ((grid_size / 15 * col) + (grid_size / 15)) + info_section_size)
    ((grid_size / 15) - 2)
    ((grid_size / 15) - 2)

let find_squ xpos ypos_o =
  let ypos = ypos_o - info_section_size in
  if xpos > -1 && xpos < grid_size && ypos > -1 && ypos < grid_size then
    (xpos / (grid_size / 15), 14 - (ypos / (grid_size / 15)))
  else (-1, -1)

let rec reset_turn (changes : (string * int * int) list ref) =
  match !changes with
  | [] -> ()
  | (ch, x, y) :: t ->
      set_letter x y board ch;
      set_was_blank x y board false;
      changes := t;
      reset_turn changes

(**[was_empty] is false if the cell at a, b was originally a letter *)
let was_empty a b = played_at board a b = false

(**[play_tiles] marks all tiles at values in backpointers to played. *)
let rec play_tiles backpointers_l =
  match backpointers_l with
  | [] -> None
  | h :: t -> (
      match h with
      | _, c1, c2 ->
          play_letter c1 c2 board;
          play_tiles t)

(** [remove_tile] removes a tile with "letter" from the tile list and returns
    the new list. *)
let rec remove_tile lst letter =
  match lst with
  | [] -> []
  | h :: t -> if h = letter then t else h :: remove_tile t letter

(**[draw_tiles] draws tiles from tile_bag until tile_lst has 7 letters. *)
let draw_tiles tile_lst tile_bag =
  if List.length tile_bag = 0 then (tile_lst, tile_bag)
  else
    let new_tile_lst = ref tile_lst in
    let new_tile_bag = ref tile_bag in
    while List.length !new_tile_lst < 7 && List.length !new_tile_bag > 0 do
      let index = Random.int (List.length !new_tile_bag) in
      let tile = List.nth !new_tile_bag index in
      new_tile_bag := remove_tile !new_tile_bag tile;
      new_tile_lst := !new_tile_lst @ [ tile ]
    done;
    (!new_tile_lst, !new_tile_bag)

(** [final_Extra_points] calculates the extra points added when player ends with
    tiles_lst tiles *)
let rec final_extra_points tiles_lst p =
  match tiles_lst with
  | [] -> p
  | h :: t ->
      let points = point_val h in
      final_extra_points t (p + points)

let p1_points = ref 0
let p2_points = ref 0
let p1_tiles : string list ref = ref []
let p2_tiles : string list ref = ref []
let tiles_bag : string list ref = ref []
let selected : (int * int) ref = ref (-1, -1)
let backpointers : (string * int * int) list ref = ref []
let player1 = ref true
let tiles_backpointer : string list ref = ref []
let game_over = ref false
let menu_open = ref false
let between_turns = ref false

let init_vars p1_tiles_init p2_tiles_init tiles_bag_init =
  p1_tiles := p1_tiles_init;
  tiles_backpointer := !p1_tiles;
  p2_tiles := p2_tiles_init;
  tiles_bag := tiles_bag_init

let enter_cell letter = function
  | -1, -1 -> ()
  | a, b ->
      let curr_lst = if !player1 then !p1_tiles else !p2_tiles in
      if was_empty a b && List.mem (String.uppercase_ascii letter) curr_lst then (
        (* only changing the cell if there is not already a tile played
           there. *)
        if letter_at board a b <> "" then (
          let tile_to_return =
            if was_blank_at board a b then "-" else letter_at board a b
          in
          if was_blank_at board a b then set_was_blank a b board false;
          if
            (* user already guessed here, return that tile to their tiles *)
            !player1
          then p1_tiles := !p1_tiles @ [ tile_to_return ]
          else p2_tiles := !p2_tiles @ [ tile_to_return ])
        else backpointers := (letter_at board a b, a, b) :: !backpointers;
        set_letter a b board (String.uppercase_ascii letter);
        if !player1 then
          p1_tiles := remove_tile !p1_tiles (String.uppercase_ascii letter)
        else p2_tiles := remove_tile !p2_tiles (String.uppercase_ascii letter))

let end_game () =
  game_over := true;

  if List.length !p1_tiles = 0 then (
    p1_points := !p1_points + final_extra_points !p2_tiles 0;
    p2_points := !p1_points - final_extra_points !p2_tiles 0)
  else (
    p2_points := !p2_points + final_extra_points !p1_tiles 0;
    p1_points := !p1_points - final_extra_points !p1_tiles 0);

  let s =
    if !p1_points > !p2_points then
      "Player 1 wins!\n\nPlayer 1 points: " ^ string_of_int !p1_points
      ^ "\nPlayer 2 points: " ^ string_of_int !p2_points
    else if !p2_points > !p1_points then
      "Player 2 wins!\n\nPlayer 2 points: " ^ string_of_int !p2_points
      ^ "\nPlayer 1 points: " ^ string_of_int !p1_points
    else
      "Tie!\n\nPlayer 1 points: " ^ string_of_int !p1_points
      ^ "\nPlayer 2 points: " ^ string_of_int !p2_points
  in
  Unix.sleepf 2.0;
  print_endline "GAME OVER!";
  print_endline s

let try_guess () =
  let points = eval_guess board !backpointers in
  if points >= 0 then (
    let _ = play_tiles !backpointers in
    if !player1 then (
      paint_switch_turns !player1;
      between_turns := true;
      player1 := false;
      p1_points := !p1_points + points;
      tiles_backpointer := !p2_tiles;
      let tl, tb = draw_tiles !p1_tiles !tiles_bag in
      p1_tiles := tl;
      tiles_bag := tb)
    else (
      paint_switch_turns !player1;
      between_turns := true;
      player1 := true;
      p2_points := !p2_points + points;
      tiles_backpointer := !p1_tiles;
      let tl, tb = draw_tiles !p2_tiles !tiles_bag in
      p2_tiles := tl;
      tiles_bag := tb);
    if
      List.length !tiles_bag = 0
      && (List.length !p2_tiles = 0 || List.length !p1_tiles = 0)
    then end_game ())
  else (
    reset_turn backpointers;
    if !player1 then p1_tiles := !tiles_backpointer
    else p2_tiles := !tiles_backpointer);

  backpointers := []

let try_blank () =
  let r = fst !selected in
  let c = snd !selected in

  if was_empty r c then (
    let old_letter = letter_at board r c in

    let has_blank = ref false in
    if !player1 then has_blank := List.mem "-" !p1_tiles
    else has_blank := List.mem "-" !p2_tiles;

    if !has_blank then (
      (if letter_at board r c <> "" then
         let tile_to_return =
           if was_blank_at board r c then "-" else letter_at board r c
         in
         if
           (* user already guessed here, return that tile to their tiles *)
           !player1
         then p1_tiles := !p1_tiles @ [ tile_to_return ]
         else p2_tiles := !p2_tiles @ [ tile_to_return ]);
      set_letter r c board "-";
      set_was_blank r c board true;
      print_board board;
      if !player1 then p1_tiles := remove_tile !p1_tiles "-"
      else p2_tiles := remove_tile !p2_tiles "-";
      let curr_tiles = if !player1 then !p1_tiles else !p2_tiles in
      print_info !p1_points !p2_points !player1 curr_tiles !tiles_bag;

      let letter_entered = ref false in
      let letter_ref = ref "-" in
      while !letter_entered = false do
        let e = wait_next_event [ Key_pressed ] in
        if e.keypressed then
          let letter = String.make 1 e.key in
          if List.mem letter alphabet then (
            letter_entered := true;
            letter_ref := letter)
      done;

      backpointers := (old_letter, r, c) :: !backpointers;
      set_letter r c board (String.uppercase_ascii !letter_ref);
      print_board board))

(** [remove_index] removes an elemennt with index from the list and returns the
    new list. *)
let remove_index element lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> if x = element then aux acc xs else aux (x :: acc) xs
  in
  aux [] lst

let paint_swap_tiles curr_tiles selected_indices =
  set_color white;
  let gap = 10 in
  let ts = 10 in
  fill_rect 0 0 (grid_size - (gap / 2)) (info_section_size - (gap / 2));
  moveto gap (info_section_size - (2 * gap));
  set_color black;
  set_text_size 80;
  draw_string "SWAP TILES";
  let y = info_section_size - ((2 * gap) + ts) - ((2 * gap) + ts) in
  let cs = grid_size / 15 in
  let _ =
    print_tiles_list curr_tiles 116 (y - ts - cs) cs gap selected_indices
  in
  moveto gap (info_section_size - (2 * gap) - 18);
  set_color black;
  draw_string
    "Select Tiles to Swap. Press any key once you have finished selection to \
     swap."

let select_tiles tiles_l indices =
  let lst = ref [] in
  for i = 0 to List.length indices - 1 do
    let index = List.nth indices i in
    let tile = List.nth tiles_l index in
    lst := List.append !lst [ tile ]
  done;
  !lst

(* swap_letters aux actually removes the selected letters in [tiles] from the
   current player's hand. *)
let swap_letters_aux tiles =
  (* first remove all of the selected tiles from the player's rack: *)
  for i = 0 to List.length tiles - 1 do
    let letter = List.nth tiles i in
    if !player1 then p1_tiles := remove_tile !p1_tiles letter
    else p2_tiles := remove_tile !p2_tiles letter
  done;
  (* then re-fill that player's rack: *)
  let new_t_lst, new_t_bag =
    if !player1 then draw_tiles !p1_tiles !tiles_bag
    else draw_tiles !p2_tiles !tiles_bag
  in
  if !player1 then p1_tiles := new_t_lst else p2_tiles := new_t_lst;
  tiles_bag := new_t_bag;
  (* then add the removed tiles to the tiles bag: *)
  for i = 0 to List.length tiles - 1 do
    let letter = List.nth tiles i in
    tiles_bag := List.append !tiles_bag [ letter ]
  done

(* swap tiles runs all of the other functions needed for a user to swap tiles *)
let rec swap_tiles tiles_lst indices_selected =
  paint_swap_tiles tiles_lst !indices_selected;
  let e = wait_next_event [ Button_down; Key_pressed ] in

  if e.keypressed then (
    let selected_tiles = select_tiles tiles_lst !indices_selected in
    if
      List.length selected_tiles > 0
      && List.length selected_tiles <= List.length !tiles_bag
    then swap_letters_aux selected_tiles)
  else if e.button then (
    let xpos = e.mouse_x in
    let ypos = e.mouse_y in
    let ti = get_tile_index xpos ypos in
    if ti >= 0 && ti <= List.length tiles_lst then
      if List.mem ti !indices_selected then
        indices_selected := remove_index ti !indices_selected
      else indices_selected := List.append !indices_selected [ ti ];
    paint_swap_tiles tiles_lst !indices_selected;
    swap_tiles tiles_lst indices_selected)

(** before_changes represents the changes required to get the board back to
    before the new letters are inputted. *)
let rec loop () : unit =
  draw_grid;
  print_board board;
  let tile_list = if !player1 then !p1_tiles else !p2_tiles in
  if !between_turns = false then
    print_info !p1_points !p2_points !player1 tile_list !tiles_bag;
  paint_outline !selected;
  print_menu_bar !menu_open;

  let e = wait_next_event [ Button_down; Key_pressed ] in

  if !menu_open then (
    let xpos = e.mouse_x in
    let ypos = e.mouse_y in
    if
      xpos >= grid_size - menu_bar_size && ypos >= grid_size + info_section_size
    then (
      paint_close_menu ();
      menu_open := false))
  else if !between_turns then (
    between_turns := false;
    print_info !p1_points !p2_points !player1 tile_list !tiles_bag)
  else if e.keypressed then (
    let letter = String.make 1 e.key in
    (* They entered a letter, so we put it on the board. *)
    if List.mem letter alphabet then enter_cell letter !selected
    else if letter = "-" then try_blank ()
    else if letter = "/" then
      (* They entered their guess, check if it is valid. If it is, play it,
         otherwise reset.*)
      try_guess ()
      (* They are canceling turn, so we undo their placed letters. *)
    else if letter = " " then (
      reset_turn backpointers;
      if !player1 then p1_tiles := !tiles_backpointer
      else p2_tiles := !tiles_backpointer))
  else if e.button then (
    let xpos = e.mouse_x in
    let ypos = e.mouse_y in
    if xpos <= menu_button_width && ypos >= grid_size then (
      (* menu: *)
      menu_open := true;
      print_menu_bar !menu_open)
    else if xpos <= (menu_button_width * 2) + 2 && ypos >= grid_size then (
      (* pass: *)
      reset_turn backpointers;
      if !player1 then p1_tiles := !tiles_backpointer
      else p2_tiles := !tiles_backpointer;
      paint_switch_turns !player1;
      if !player1 then player1 := false else player1 := true;
      between_turns := true)
    else if xpos <= (menu_button_width * 3) + 3 && ypos >= grid_size then (
      (* swap:*)
      reset_turn backpointers;
      if !player1 then p1_tiles := !tiles_backpointer
      else p2_tiles := !tiles_backpointer;
      let curr_tiles = if !player1 then !p1_tiles else !p2_tiles in
      print_info !p1_points !p2_points !player1 curr_tiles !tiles_bag;
      swap_tiles curr_tiles (ref []);
      paint_switch_turns !player1;
      if !player1 then player1 := false else player1 := true;
      between_turns := true)
    else
      let cell = find_squ xpos ypos in
      if cell <> !selected then selected := cell;
      print_board board;
      print_info !p1_points !p2_points !player1 [] !tiles_bag;
      paint_outline !selected);

  if !game_over = false then loop ()
