open Graphics
open Gameboard

let dict = Arg.read_arg (project_root () ^ "/data/dictionary.txt")

module StringSet = Set.Make (struct
  type t = string

  let compare a b =
    compare (String.lowercase_ascii a) (String.lowercase_ascii b)
end)

let dict = StringSet.of_list (Array.to_list dict)
let valid_word str = StringSet.mem str dict

open Gameboard

let convert_assoc csv =
  let lst = Csv.load csv in
  let letter = List.hd lst in
  let points = List.map int_of_string (List.hd (List.tl lst)) in
  List.combine letter points

(**[point_val] is the point value of the letter entered:*)
let point_val l =
  if List.mem_assoc l (convert_assoc (project_root () ^ "/data/points.csv"))
  then List.assoc l (convert_assoc (project_root () ^ "/data/points.csv"))
  else 0

(** [loop_guess board lst (cols,rows)] inputs a list of letters with
    coordinates, and returns a pair of lists of the columns and rows that they
    are in. *)
let rec loop_guess board (lst : (string * int * int) list) (cols_lst, rows_lst)
    =
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

(** [get_word x y len board v] is a tuple of (valid, word, start col, start row,
    vertical) where valid is true if there are no gaps and word is the string
    entered, start col and start row are the beginning of the entered string,
    and vertical is true if the word is vertical *)
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

(** check_before checks if there are any letters before the current word and if
    so, adds them to the beginning of the word.*)
let check_before s_col s_row this_col this_row full_word vertical board =
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
  done

(** check_after checks if there are any letters after the current word and if
    so, adds them to the end of the word.*)
let check_after this_col this_row full_word vertical board : unit =
  let i = ref 1 in
  while !i > 0 do
    if vertical then this_row := !this_row + 1 else this_col := !this_col + 1;
    if !this_row <= 14 && !this_col <= 14 then
      if played_at board !this_col !this_row then
        full_word := !full_word ^ letter_at board !this_col !this_row
      else i := 0
    else i := 0
  done

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
  check_before s_col s_row this_col this_row full_word vertical board;

  this_col := start_col;
  this_row := start_row;
  if vertical then this_row := start_row + String.length word - 1
  else this_col := start_col + String.length word - 1;

  (* iterate below word: *)
  check_after this_col this_row full_word vertical board;
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

(** [get_all_words] returns the list of the user's guess - <0 if it is not a
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

(** [grade_aux wmult pts base] augments the word multiplier and the word points
    based on the multiplier and letter for the current box*)
let grade_aux word_multiplier points this_base_points = function
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

(**[get_points] get the point value of the word [word] which starts at col: [sc]
   and row: [sr]*)
let get_points word sc sr vert board =
  let points = ref 0 in
  let word_multiplier = ref 1 in

  for i = 0 to String.length word - 1 do
    let this_row = if vert then sr + i else sr in
    let this_col = if vert then sc else sc + i in
    let this_letter = letter_at board this_col this_row in
    let this_base_points =
      if was_blank_at board this_col this_row then 0 else point_val this_letter
    in

    if played_at board this_col this_row then
      points := !points + this_base_points
    else
      let mult = multiplier_at board this_row this_col in
      grade_aux word_multiplier points this_base_points mult
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
            if valid_word word then
              let this_points = get_points word sc sr vert board in
              eval_all_words t (points + this_points) board
            else eval_all_words t (-1) board)

(** [check_over_star] checks if any of the words in [words_lst] go over the star
    multiplier. *)
let rec check_over_star words_lst b board =
  match words_lst with
  | [] -> b
  | h :: t -> (
      if (* already went over star, don't need to check: *)
         b then true
      else
        match h with
        | word, sc, sr, vert ->
            let b_ref = ref false in
            for i = 0 to String.length word - 1 do
              let this_row = if vert then sr + i else sr in
              let this_col = if vert then sc else sc + i in
              match multiplier_at board this_row this_col with
              | Star -> b_ref := true
              | _ -> b_ref := !b_ref
            done;
            check_over_star t !b_ref board)

(** [check_connecting] checks if any of the words in [words_lst] are connected
    to an existing word (contain at least one already played letter). *)
let rec check_connecting words_lst b board =
  match words_lst with
  | [] -> b
  | h :: t -> (
      if (* already know it is connecting, don't need to check: *)
         b then true
      else
        match h with
        | word, sc, sr, vert ->
            let b_ref = ref false in
            for i = 0 to String.length word - 1 do
              let this_row = if vert then sr + i else sr in
              let this_col = if vert then sc else sc + i in
              if played_at board this_col this_row then b_ref := true
            done;
            check_connecting t !b_ref board)

(** [checks_connectng_aux] runs the helper functions to check if any of the
    words in word_lst are connected to an existing word or if the board is
    empty, that the user played across a star. *)
let check_connecting_aux lst board =
  if is_empty board then check_over_star lst false board
  else check_connecting lst false board

(**[get_guess_lst] returns the list of letters at the positions in spaces_lst *)
let rec get_guess_lst spaces acc board =
  match spaces with
  | [] -> acc
  | h :: t -> (
      match h with
      | _, r, c ->
          let tup = (letter_at board r c, r, c) in
          if List.mem tup acc then get_guess_lst t acc board
          else get_guess_lst t (acc @ [ tup ]) board)

let rec pr_array = function
  | [] -> ()
  | (str, _, _, _) :: t ->
      print_string (str ^ ",");
      pr_array t

(** [eval_guess] returns the point value of the user's guess - <-1 if it is not
    a valid guess*)
let eval_guess (board : Gameboard.t)
    (spaces_lst : (string * color * color) list) =
  let guess_lst = get_guess_lst spaces_lst [] board in
  match get_all_words board guess_lst with
  | None -> -1
  | Some words_lst ->
      let _ = pr_array words_lst in
      if List.length words_lst = 0 then -1
      else if
        (* check if it was a valid connecting move, if so calculate points *)
        check_connecting_aux words_lst board
      then eval_all_words words_lst 0 board
      else -1
