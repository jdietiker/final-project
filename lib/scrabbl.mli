open Graphics

val valid_word : string -> bool
(* val grade_word : string -> int *)

val eval_guess : Gameboard.t -> (string * color * color) list -> int
(**[valid_guess board guess_lst] is true if the word entered through [guess_lst]
   is a valid word and in a line. *)

val point_val : string -> color
(**[point_val] is the point value of the letter entered:*)
