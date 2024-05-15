(** @author Lea Duesterwald lkd46, Jasmine Dietiker jfd237, Boaz Ng bn229*)

open Final_project.Gui

let init_tile_bag =
  Array.to_list
    (Arg.read_arg "/Users/leaduesterwald/CS_3110/final-project/data/bag.txt")

let p1_tiles_i, bag1 = draw_tiles [] init_tile_bag
let p2_tiles_i, tiles_bag_i = draw_tiles [] bag1
let () = init_vars p1_tiles_i p2_tiles_i tiles_bag_i
let () = print_endline "Hello, World!"
let () = loop ()
