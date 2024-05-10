open Final_project.Gui
open Final_project.Gameboard

let () = print_endline "Hello, World!"

(* let board = init *)
let p1_tiles = [ "A"; "B"; "C"; "D"; "E"; "F"; "G" ]
let p2_tiles = p1_tiles
let tiles_bag = p1_tiles

let () =
  loop
    (ref (-1, -1))
    (ref []) (ref 0) (ref 0) (ref p1_tiles) (ref p2_tiles) (ref tiles_bag)
    (ref true)
