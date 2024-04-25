open Graphics
open Gameboard

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

let rec draw_grid_h_aux = function
  | [] -> ()
  | h :: t ->
      let a, b = Graphics.current_point () in
      Graphics.moveto 0 h;
      Graphics.lineto grid_size h;
      draw_grid_h_aux t;
      Graphics.moveto a b

let rec draw_grid_v_aux = function
  | [] -> ()
  | h :: t ->
      let a, b = Graphics.current_point () in
      Graphics.moveto h 0;
      Graphics.lineto h grid_size;
      draw_grid_v_aux t;
      Graphics.moveto a b

let draw_grid =
  Graphics.open_graph
    (" " ^ string_of_int grid_size ^ "x" ^ string_of_int grid_size);
  set_window_title "SCRABBLE!";
  set_color black;
  set_text_size 200;
  draw_grid_h_aux coordinates;
  draw_grid_v_aux coordinates

(** prints the ith cell in the jth column of board. *)
let print_cell i j s =
  let a, b = text_size s in
  moveto
    ((grid_size / 15 * i) + (grid_size / 30) - (a / 2) + 1)
    (grid_size - ((grid_size / 15 * j) + (grid_size / 30) + (b / 2) + 2));
  set_text_size 50;
  set_color black;
  draw_string s

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

let print_cell_color i j (el : Gameboard.elt) =
  let color =
    match el_multiplier el with
    | No -> if el_letter el = "" then white else yellow
    | TW -> if el_letter el = "" then red else yellow
    | DW -> if el_letter el = "" then green else yellow
    | TL -> if el_letter el = "" then blue else yellow
    | DL -> if el_letter el = "" then cyan else yellow
    | Star -> if el_letter el = "" then magenta else yellow
  in
  set_color color;
  fill_rect
    ((grid_size / 15 * i) + 1)
    (grid_size - ((grid_size / 15 * j) + (grid_size / 15)))
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
    (grid_size - ((grid_size / 15 * col) + (grid_size / 15)))
    ((grid_size / 15) - 2)
    ((grid_size / 15) - 2)

let find_squ xpos ypos =
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

(** before_changes represents the changes required to get the board back to
    before the new letters are inputted. *)
let rec loop (selected : (int * int) ref)
    (before_changes : (string * int * int) list ref) : unit =
  draw_grid;
  print_board board;
  paint_outline !selected;

  let e = wait_next_event [ Button_down; Key_pressed ] in

  let orig_status = before_changes in
  if e.keypressed then (
    let letter = String.make 1 e.key in

    if List.mem letter alphabet then (
      match !selected with
      | -1, -1 -> ()
      | a, b ->
          orig_status := (letter_at board a b, a, b) :: !orig_status;
          set_letter a b board (String.uppercase_ascii letter))
    else if letter = "/" then orig_status := []
    else if letter = " " then reset_turn orig_status)
  else if e.button then (
    let xpos = e.mouse_x in
    let ypos = e.mouse_y in
    let cell = find_squ xpos ypos in

    if find_squ xpos ypos <> !selected then selected := cell;
    print_board board;
    paint_outline !selected);

  loop selected orig_status
