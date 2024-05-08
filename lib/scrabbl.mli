open Graphics

val valid_word : string -> bool
(* val grade_word : string -> int *)

val valid_guess : Gameboard.t -> (string * color * color) list -> bool
(**[valid_guess board guess_lst] is true if the word entered through [guess_lst]
   is a valid word and in a line. *)
