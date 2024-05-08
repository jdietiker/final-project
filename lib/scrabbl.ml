open Graphics

let dict = Arg.read_arg "data/dictionary.txt"

module StringSet = Set.Make (struct
  type t = string

  let compare a b =
    compare (String.lowercase_ascii a) (String.lowercase_ascii b)
end)

let dict = StringSet.of_list (Array.to_list dict)
let valid_word str = StringSet.mem str dict

open Gameboard

(** gets the integer value of the color *)
let get_cell_val (c : color) : string =
  if c = 0 then "0"
  else if c = 1 then "1"
  else if c = 2 then "2"
  else if c = 3 then "3"
  else if c = 4 then "4"
  else if c = 5 then "5"
  else if c = 6 then "6"
  else if c = 7 then "7"
  else if c = 8 then "8"
  else if c = 9 then "9"
  else if c = 10 then "10"
  else if c = 1 then "11"
  else if c = 2 then "12"
  else if c = 3 then "13"
  else if c = 4 then "14"
  else "15"

let rec loop_guess lst =
  match lst with
  | [] -> 0
  | h :: t -> begin
      match h with
      | _, c, r ->
          print_endline
            ("IN LOOP GUES _ S = " ^ "TODO" ^ "  col: " ^ get_cell_val c
           ^ "  row: " ^ get_cell_val r);
          loop_guess t
    end

let valid_guess (guess_lst : (string * color * color) list) =
  let _ = loop_guess guess_lst in
  true
(* need to first check if they're all in a line (all have same horizontal or
   same vertical) *)

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
