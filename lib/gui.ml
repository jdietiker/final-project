open Graphics
open Gameboard
open Scrabbl

let info_section_size = 120
let grid_size = 601

let alphabet =
  [
    "a";
    "b";
    "c";
    "d";
    "e";
    "f";
    "g";
    "h";
    "i";
    "j";
    "k";
    "l";
    "m";
    "n";
    "o";
    "p";
    "q";
    "r";
    "s";
    "t";
    "u";
    "v";
    "w";
    "x";
    "y";
    "z";
  ]

let board = init

(** ex: [generate_coord 15 (600/15)] =
    [[0;40;80;120;160;200;240;280;320;360;400;440;480;520;560;600]] *)
let rec generate_coord num int =
  if num = 0 then [ 0 ] else generate_coord (num - 1) int @ ((num * int) :: [])

let coordinates = generate_coord 15 (grid_size / 15)

let rec print_tiles_list lst x y cell_size gap_size =
  match lst with
  | [] -> 0
  | h :: t ->
      let c = Graphics.rgb 237 210 153 in
      set_color c;
      fill_rect x y cell_size cell_size;
      set_color black;
      draw_rect x y cell_size cell_size;

      let a, b = text_size h in
      moveto
        (x + (cell_size / 2) - (a / 2) + 1)
        (y + ((cell_size / 2) - (b / 2)) + 2);
      set_text_size 60;
      draw_string h;

      print_tiles_list t (x + cell_size + gap_size) y cell_size gap_size

(** prints the information about the current state of the game in the info
    section. *)
let print_info p1_points p2_points player1 curr_tiles =
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
   ^ string_of_int p2_points);

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
  let _ = print_tiles_list curr_tiles 116 (!y - ts - cs) cs gap in

  moveto gap (!y - (2 * gap));
  set_color black;
  draw_string "Current tiles: "

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
    ^ string_of_int (grid_size + info_section_size));
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
  let points = if point_val s = 0 then "" else string_of_int (point_val s) in
  moveto
    ((grid_size / 15 * i) + (grid_size / 30) - (a / 2) + 1)
    (grid_size
    - ((grid_size / 15 * j) + (grid_size / 30) + (b / 2) + 2)
    + info_section_size);
  set_color black;
  set_text_size 60;
  draw_string s;

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
      changes := t;
      reset_turn changes

let check_word (changes : (string * int * int) list ref) =
  let wordpos =
    List.sort_uniq
      (fun (_, x1, y1) (_, x2, y2) ->
        if x1 > x2 then 1
        else if x1 < x2 then -1
        else if y1 > y2 then 1
        else if y1 < y2 then -1
        else 0)
      !changes
  in
  match wordpos with
  | [] -> false
  | (_, xh, yh) :: _ ->
      if List.filter (fun (_, x, _) -> x = xh) wordpos = wordpos then
        (* all in one column *)
        (* NOTE TO SELF: be careful if they skip a cell!!!! *)
        failwith "TODO"
      else if List.filter (fun (_, _, y) -> y = yh) wordpos = wordpos then
        (* all in one row *)
        failwith "TODO"
      else false

(**[was_empty] is false if the cell at a, b was originally a letter *)
let rec was_empty a b = played_at board a b = false

(**[play_tiles] marks all tiles at values in backpointers to played. *)
let rec play_tiles backpointers_l =
  match backpointers_l with
  | [] -> None
  | h :: t -> (
      match h with
      | _, c1, c2 ->
          play_letter c1 c2 board;
          play_tiles t)

(** before_changes represents the changes required to get the board back to
    before the new letters are inputted. *)
let rec loop (selected : (int * int) ref)
    (backpointers : (string * int * int) list ref) p1_points p2_points p1_tiles
    p2_tiles tiles_bag player1 : unit =
  draw_grid;
  print_board board;
  let tile_list = if !player1 then !p1_tiles else !p2_tiles in
  print_info !p1_points !p2_points !player1 tile_list;
  paint_outline !selected;

  let e = wait_next_event [ Button_down; Key_pressed ] in

  if e.keypressed then (
    let letter = String.make 1 e.key in

    if List.mem letter alphabet then (
      match !selected with
      | -1, -1 -> ()
      | a, b ->
          if was_empty a b then (
            (* only changing the cell if there is not already a tile played
               there. *)
            backpointers := (letter_at board a b, a, b) :: !backpointers;
            set_letter a b board (String.uppercase_ascii letter)))
    else if letter = "/" then (
      (* They entered their gues, check if it is valid. If it is, play it,
         otherwise reset.*)
      let points = eval_guess board !backpointers in
      if points >= 0 then (
        let _ = play_tiles !backpointers in
        if !player1 then (
          player1 := false;
          p1_points := !p1_points + points)
        else (
          player1 := true;
          p2_points := !p2_points + points);
        print_endline ("Total points earned: " ^ string_of_int points))
      else reset_turn backpointers;

      backpointers := [])
    else if letter = " " then reset_turn backpointers)
  else if e.button then (
    let xpos = e.mouse_x in
    let ypos = e.mouse_y in
    let cell = find_squ xpos ypos in
    if find_squ xpos ypos <> !selected then selected := cell;
    print_board board;
    print_info !p1_points !p2_points !player1 [];
    paint_outline !selected);

  loop selected backpointers p1_points p2_points p1_tiles p2_tiles tiles_bag
    player1
