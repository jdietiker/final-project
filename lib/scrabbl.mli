open Graphics

val valid_word : string -> bool
(* val grade_word : string -> int *)

val valid_guess : (string * color * color) list -> bool
(**[valid_guess] is true if the word entered through [guess_lst] is a valid word
   and in a line. *)
