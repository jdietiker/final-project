type multiplier =
  | TW
  | DW
  | TL
  | DL
  | Star
  | No

type elt
(** [elt] is the type of cells in the board 2d array. Each elt contains
    information about the kind of multiplier in the space and the letter played
    (or ' ' if no letter is played)*)

type t = elt array array
(** [t] is the type of Gameboard which is represented as a 2D array of elements
    of type [elt]. The jth element of the ith sub-array is the jth column and
    the ith row *)

val empty : t
(** [empty] is an empty board. *)

val init : t
(** [init] is the gameboard inialized to its 15x15 starting state with
    multiplers and no letters played. *)

val multiplier_at : t -> int -> int -> multiplier
(** [multiplier board i j] is the multiplier stored in row [i], column [j] in
    [board] *)

val letter_at : t -> int -> int -> string
(** [letter_at board i j] is the letter stored at row [i], column [j] in [board]*)

val played_at : t -> int -> int -> bool
(**[played_at board i j] is the boolean of whether the letter at i, j has been
   played.*)

val was_blank_at : t -> int -> int -> bool
(**[was_blank_at board i j] is the boolean of whether the letter at i, j was
   blank.*)

val el_multiplier : elt -> multiplier
(** [el_multiplier el] is the multiplier stored in element [el] *)

val el_letter : elt -> string
(** [el_letter el] is the letter stored in element [el] *)

val el_played : elt -> bool
(** [el_played el] is the value of played stored in element [el] *)

val el_at : t -> int -> int -> elt
(** [el_at board i j] is the element stored stored at row [i], column [j] in
    [board] *)

val length : t -> int
(** [length board] is the length of [board] *)

val set_letter : int -> int -> t -> string -> unit
(** [set_letter r c board s] sets the letter at row [r] and col [c] of [board]
    to [s] *)

val set_was_blank : int -> int -> t -> bool -> unit
(**[set_was_blank r c boardb ] sets the letter at row [r] and col [c] of [board]
   to be [b] *)

val play_letter : int -> int -> t -> unit
(** [play_letter r c board] sets the letter at row [r] and col [c] of [board] to
    be played. *)

val is_empty : t -> bool
(** [is_empty board ] is true if there are no played tiles on the board [board]*)

val project_root : unit -> string
(**[project_root] is the directory to the root of the dune project *)
