open Graphics

val valid_word : string -> bool
(** [valid_word str] returns true if str is a valid word*)

val eval_guess : Gameboard.t -> (string * color * color) list -> int
(**[eval_guess board guess_lst] is true if the word entered through [guess_lst]
   is a valid word and in a line. *)

val point_val : string -> color
(**[point_val] is the point value of the letter entered:*)
