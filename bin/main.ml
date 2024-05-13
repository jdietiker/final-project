open Final_project.Gui
open Final_project.Gameboard

let () = print_endline "Hello, World!"
let init_tile_bag = Array.to_list (Arg.read_arg "data/bag.txt")
let p1_tiles, bag1 = draw_tiles [] init_tile_bag
let p2_tiles, tiles_bag = draw_tiles [] bag1

let () =
  loop
    (ref (-1, -1))
    (ref []) (ref 0) (ref 0) (ref p1_tiles) (ref p2_tiles) (ref tiles_bag)
    (ref true) (ref p1_tiles) (ref false)
