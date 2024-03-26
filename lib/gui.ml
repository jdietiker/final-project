open Graphics
open Gameboard

let grid_size = 601

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
  moveto
    ((grid_size / 15 * i) + (grid_size / 60))
    (grid_size - ((grid_size / 15 * j) + (grid_size / 20)));
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
      | Star -> " *"
    else el_letter el
  in
  print_cell i j s

let print_cell_color i j (el : Gameboard.elt) =
  let color =
    match el_multiplier el with
    | No -> if el_letter el = "" then white else yellow
    | TW -> red
    | DW -> green
    | TL -> blue
    | DL -> cyan
    | Star -> magenta
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

(* let print_init = print_board Gameboard.init *)

open Bogue
module W = Widget
module L = Layout

let make_layout ?(w = 120) ?(h = 90) color text =
  let text_room = Layout.resident (W.label text) in
  let style = Style.(of_bg (color_bg color)) in
  let image_room = Layout.resident (Widget.box ~w ~h ~style ()) in
  Layout.superpose [ image_room; text_room ]

let print_bogue =
  make_layout Draw.(opaque (find_color "lightcoral")) "My first house"
  |> Bogue.of_layout |> Bogue.run
