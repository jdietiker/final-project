val draw_grid : unit
val print_board : Gameboard.t -> unit

val loop :
  (int * int) ref ->
  (string * int * int) list ref ->
  int ref ->
  int ref ->
  string list ref ->
  string list ref ->
  string list ref ->
  bool ref ->
  unit
(**[loop] loops through player turns*)
