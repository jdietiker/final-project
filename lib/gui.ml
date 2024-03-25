open Graphics

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
  set_text_size 200;
  draw_grid_h_aux coordinates;
  draw_grid_v_aux coordinates

(* open Gameboard *)

(** prints the ith cell in the jth column of board. *)
(* let print_cell i j = moveto ((grid_size / 15 * i) + (grid_size / 60))
   (grid_size - ((grid_size / 15 * j) + (grid_size / 20))); set_text_size 50;
   draw_string "H" *)

(* let rec print_board board xpos ypos = match board with board.empty -> ()*)
