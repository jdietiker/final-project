val draw_grid : unit
val print_board : Gameboard.t -> unit
val draw_tiles : string list -> string list -> string list * string list

val loop :
  (int * int) ref ->
  (string * int * int) list ref ->
  int ref ->
  int ref ->
  string list ref ->
  string list ref ->
  string list ref ->
  bool ref ->
  string list ref ->
  unit
(**[loop] loops through player turns*)
