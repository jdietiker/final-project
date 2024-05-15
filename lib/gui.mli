val draw_grid : unit
(** [draw_grid] draws a grid. *)

val print_board : Gameboard.t -> unit
(** [print_board gb] prints the gb in the gui. *)

val draw_tiles : string list -> string list -> string list * string list
(** [draw_tiles playerbag bag] draws tiles from bag into playerbag until the
    playerbag has 7 tiles or bag is empty.*)

val init_vars : string list -> string list -> string list -> unit
(** [init_vars p1bag p2bag bag] initializes the game where player1 has the
    letters in [p1bag] and player 2 has the letters in [p2bag], and the game bag
    is set to [bag] *)

val loop : unit -> unit
(**[loop ()] loops through player turns*)
