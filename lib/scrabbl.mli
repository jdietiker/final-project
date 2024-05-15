open Graphics

val valid_word : string -> bool
(** [valid_word str] returns true if str is a valid word*)

val eval_guess : Gameboard.t -> (string * color * color) list -> int
(**[eval_guess board guess_lst] is the points scored by guess if the word
   entered through [guess_lst] is a valid word and in a line. Returns -1 if
   invalid. See the rulebook for evaluation rules. *)

val point_val : string -> color
(**[point_val] is the point value of the letter entered:*)
