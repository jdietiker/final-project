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

val char_at : t -> int -> int -> char
(** [char_at board i j] is the character stored at row [i], column [j] in
    [board]*)
