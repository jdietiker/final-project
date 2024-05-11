open Final_project.Gui
open Final_project.Gameboard

let () = print_endline "Hello, World!"

let init_tile_bag =
  [
    "A";
    "A";
    "A";
    "A";
    "A";
    "A";
    "A";
    "A";
    "A";
    "B";
    "B";
    "C";
    "C";
    "D";
    "D";
    "D";
    "D";
    "E";
    "E";
    "E";
    "E";
    "E";
    "E";
    "E";
    "E";
    "E";
    "E";
    "E";
    "E";
    "F";
    "F";
    "G";
    "G";
    "G";
    "H";
    "H";
    "I";
    "I";
    "I";
    "I";
    "I";
    "I";
    "I";
    "I";
    "I";
    "J";
    "K";
    "L";
    "L";
    "L";
    "L";
    "M";
    "M";
    "N";
    "N";
    "N";
    "N";
    "N";
    "N";
    "O";
    "O";
    "O";
    "O";
    "O";
    "O";
    "O";
    "O";
    "P";
    "P";
    "Q";
    "R";
    "R";
    "R";
    "R";
    "R";
    "R";
    "S";
    "S";
    "S";
    "S";
    "T";
    "T";
    "T";
    "T";
    "T";
    "T";
    "U";
    "U";
    "U";
    "U";
    "V";
    "V";
    "W";
    "W";
    "X";
    "Y";
    "Y";
    "Z";
  ]

let p1_tiles, bag1 = draw_tiles [] init_tile_bag
let p2_tiles, tiles_bag = draw_tiles [] bag1

let () =
  loop
    (ref (-1, -1))
    (ref []) (ref 0) (ref 0) (ref p1_tiles) (ref p2_tiles) (ref tiles_bag)
    (ref true) (ref p1_tiles)
