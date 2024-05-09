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

(**[point_val] is the point value of the letter entered:*)
let point_val l =
  if l = "A" then 1
  else if l = "B" then 3
  else if l = "C" then 3
  else if l = "D" then 2
  else if l = "E" then 1
  else if l = "F" then 4
  else if l = "G" then 2
  else if l = "H" then 4
  else if l = "I" then 1
  else if l = "J" then 8
  else if l = "K" then 5
  else if l = "L" then 1
  else if l = "M" then 3
  else if l = "N" then 1
  else if l = "O" then 1
  else if l = "P" then 3
  else if l = "Q" then 10
  else if l = "R" then 1
  else if l = "S" then 1
  else if l = "T" then 1
  else if l = "U" then 1
  else if l = "V" then 4
  else if l = "W" then 4
  else if l = "X" then 8
  else if l = "Y" then 4
  else 10

let rec loop_guess board (lst : (string * color * color) list)
    (cols_lst, rows_lst) =
  match lst with
  | [] -> (cols_lst, rows_lst)
  | h :: t -> begin
      match h with
      | _, c, r ->
          let cols_lst_new =
            if List.mem c cols_lst = false then List.append cols_lst [ c ]
            else cols_lst
          in
          let rows_lst_new =
            if List.mem r rows_lst = false then List.append rows_lst [ r ]
            else rows_lst
          in
          loop_guess board t (cols_lst_new, rows_lst_new)
    end

(** [min_value] gets the minimum value of the color list*)
let min_value lst =
  match lst with
  | [] -> -1
  | hd :: tl -> List.fold_left min hd tl

(** [get_word] is a tuple of (valid, word) where valid is true if there are no
    gaps and word is the string entered *)
let get_word start_col start_row length board (vertical : bool) =
  let this_col = ref start_col in
  let this_row = ref start_row in
  let no_gaps = ref true in
  let return_string = ref "" in
  let i = ref 0 in
  while !i < length do
    (* if there is already a letter there, want to iterate one more time: *)
    if played_at board !this_col !this_row then i := !i - 1;
    let l = letter_at board !this_col !this_row in
    if l = "" then no_gaps := false else return_string := !return_string ^ l;
    if vertical then this_row := !this_row + 1 else this_col := !this_col + 1;
    if !this_col > 15 || !this_row > 15 then no_gaps := false;
    i := !i + 1
  done;
  (!no_gaps, !return_string, start_col, start_row, vertical)

(** [valid_pos] checks if the user guess under guess_lst is in the valid
    position (all letters are in one line and there are no gaps) *)
let valid_pos board guess_lst =
  let cols, rows = loop_guess board guess_lst ([], []) in
  if List.length cols <> 1 && List.length rows <> 1 then (false, "", 0, 0, false)
  else
    (* check for gaps: *)
    let min_col = min_value cols in
    let min_row = min_value rows in
    let len = List.length guess_lst in
    if List.length cols = 1 then get_word min_col min_row len board true
    else get_word min_col min_row len board false

(**[extend_word] iterates above and below the word to add any existing
   connecting tiles. returns (full_word, start_c, start_r, vert) which is the
   full word, the starting column, starting row, and whether it is vertical *)
let extend_word start_col start_row word vertical board =
  let s_col = ref start_col in
  let s_row = ref start_row in
  let full_word = ref word in

  let this_col = ref start_col in
  let this_row = ref start_row in
  (* iterate above word: *)
  let i = ref 1 in
  while !i > 0 do
    if vertical then this_row := !this_row - 1 else this_col := !this_col - 1;
    if !this_row >= 0 && !this_col >= 0 then
      if played_at board !this_col !this_row then (
        s_col := !this_col;
        s_row := !this_row;
        full_word := letter_at board !this_col !this_row ^ !full_word)
      else i := 0
    else i := 0
  done;

  this_col := start_col;
  this_row := start_row;
  if vertical then this_row := start_row + String.length word - 1
  else this_col := start_col + String.length word - 1;

  (* iterate below word: *)
  let i = ref 1 in
  while !i > 0 do
    if vertical then this_row := !this_row + 1 else this_col := !this_col + 1;
    if !this_row <= 14 && !this_col <= 14 then
      if played_at board !this_col !this_row then
        full_word := !full_word ^ letter_at board !this_col !this_row
      else i := 0
    else i := 0
  done;
  (* return final value: *)
  (!full_word, !s_col, !s_row)

(**[check_opp] checks all the user's guesses in the opposite direction to see if
   there are ny additional words they got. *)
let rec check_opp board guess_lst current_vert word_lst =
  let v = if current_vert then false else true in
  match guess_lst with
  | [] -> word_lst
  | h :: t -> (
      match h with
      | _, c1, c2 ->
          let l = letter_at board c1 c2 in
          let full_word, new_sc, new_sr = extend_word c1 c2 l v board in
          let new_lst =
            if String.length full_word > 1 then
              List.append [ (full_word, new_sc, new_sr, v) ] word_lst
            else word_lst
          in
          check_opp board t current_vert new_lst)

(** [eval_guess] returns the point value of the user's guess - <0 if it is not a
    valid guess*)
let get_all_words (board : Gameboard.t)
    (guess_lst : (string * color * color) list) =
  let words_lst = ref [] in
  match valid_pos board guess_lst with
  | valid, word, sc, sr, vert ->
      if valid = false then None
      else if String.length word <> 1 then (
        let full_word, new_sc, new_sr = extend_word sc sr word vert board in
        words_lst :=
          List.append [ (full_word, new_sc, new_sr, vert) ] !words_lst;
        (* check in the other direction for every letter: *)
        words_lst := check_opp board guess_lst vert !words_lst;
        Some !words_lst)
      else
        (* string is length 1: *)
        (* checking vertical direction*)
        let full_word_v, v_sc, v_sr = extend_word sc sr word true board in
        if String.length full_word_v > 1 then
          words_lst :=
            List.append [ (full_word_v, v_sc, v_sr, true) ] !words_lst;
        (* checking horizontal: *)
        let full_word_h, h_sc, h_sr = extend_word sc sr word false board in
        if String.length full_word_h > 1 then
          words_lst :=
            List.append [ (full_word_h, h_sc, h_sr, false) ] !words_lst;

        Some !words_lst

(**[get_points] get the point value of the word [word] which starts at col: [sc]
   and row: [sr]*)
let get_points word sc sr vert board =
  let points = ref 0 in
  let word_multiplier = ref 1 in

  for i = 0 to String.length word - 1 do
    let this_row = if vert then sr + i else sr in
    let this_col = if vert then sc else sc + i in
    let this_letter = letter_at board this_col this_row in
    let this_base_points = point_val this_letter in

    if played_at board this_row this_col then
      points := !points + this_base_points
    else
      match multiplier_at board this_row this_col with
      | TW ->
          word_multiplier := 3 * !word_multiplier;
          points := !points + this_base_points
      | DW ->
          word_multiplier := 2 * !word_multiplier;
          points := !points + this_base_points
      | TL -> points := !points + (3 * this_base_points)
      | DL -> points := !points + (2 * this_base_points)
      | Star ->
          word_multiplier := 2 * !word_multiplier;
          points := !points + this_base_points
      | No -> points := !points + this_base_points
  done;
  !points * !word_multiplier

(** [eval_all_words] evaluates the list of all words in this user's guess: *)
let rec eval_all_words words_lst points board =
  match words_lst with
  | [] -> points
  | h :: t -> (
      if points = -1 then -1
      else
        match h with
        | word, sc, sr, vert ->
            print_endline
              ("WORD: " ^ word ^ " SC: " ^ string_of_int sc ^ "  SR: "
             ^ string_of_int sr ^ " vert: " ^ string_of_bool vert);

            if valid_word word then
              let this_points = get_points word sc sr vert board in
              eval_all_words t (points + this_points) board
            else eval_all_words t (-1) board)

(** [eval_guess] returns the point value of the user's guess - <0 if it is not a
    valid guess*)
let eval_guess (board : Gameboard.t) (guess_lst : (string * color * color) list)
    =
  match get_all_words board guess_lst with
  | None -> -1
  | Some words_lst ->
      if List.length words_lst = 0 then -1
      else (
        print_endline "ALL VALID WORDS: ";
        let points = eval_all_words words_lst 0 board in
        points)

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
