let dict = Arg.read_arg "data/dictionary.txt"

module StringSet = Set.Make (struct
  type t = string

  let compare a b =
    compare (String.lowercase_ascii a) (String.lowercase_ascii b)
end)

let dict = StringSet.of_list (Array.to_list dict)
let valid_word str = StringSet.mem str dict

open Gameboard

(** [word_at (x, y) board vertical] returns word starting from [(x,y)] in
    [board] going down if [vertical] is true, and going across if [vertical] is
    false. It stops recording the word if there's a gap or reaches the end of
    the board.*)
let rec word_at (x, y) (board : Gameboard.t) (vertical : bool) =
  match vertical with
  | true ->
      let chr = letter_at board x y in
      if chr != "" && y != length board then chr ^ word_at (x, y + 1) board true
      else ""
  | false ->
      let chr = letter_at board x y in
      if chr != "" && x != length board then
        chr ^ word_at (x + 1, y) board false
      else ""

(** [grade_word_aux (x,y) board vertical] grades the word in [board] starting
    from coordinates [(x, y)] and going either down or across, depending on
    [vertical], and returns the int corresponding to its score. (or 0 if the
    word ends or the board ends).*)
let grade_word_aux (x, y) (board : Gameboard.t) (vertical : bool) =
  match vertical with
  | true -> begin
      let chr, mult = (letter_at board x y, multiplier_at board x y) in
      if chr = "" || x = length board then 0
      else
        match mult with
        | TW -> 0
        | DW -> 0
        | TL -> 0
        | DL -> 0
        | Star -> 0
        | No -> 0
    end
  | false -> 1

(** INCOMPLETE SORRY GUYS *)
let grade_word (x, y) (board : Gameboard.t) (vertical : bool) =
  match vertical with
  | true -> if not (valid_word (word_at (x, y) board true)) then 0 else 0
  | false -> 1
