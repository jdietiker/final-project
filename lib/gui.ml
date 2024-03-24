open Graphics

let (vertical_coordinates : int list) =
  [ 0; 20; 40; 60; 80; 100; 120; 140; 160; 180; 200; 220; 240; 260; 280; 300 ]

let open_grid = open_graph "300x300"

let rec draw_grid_h_aux = function
  | [] -> ()
  | h :: t ->
      let a, b = Graphics.current_point () in
      Graphics.moveto 0 h;
      Graphics.lineto 300 h;
      draw_grid_h_aux t;
      Graphics.moveto a b

let draw_grid_h = draw_grid_h_aux vertical_coordinates
