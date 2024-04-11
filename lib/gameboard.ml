type multiplier =
  | TW
  | DW
  | TL
  | DL
  | Star
  | No

type elt = multiplier * string
type t = elt array array

let empty : t =
  [|
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
  |]

let init : t =
  [|
    [|
      (TW, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (TW, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (TW, "");
    |];
    [|
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
    |];
    [|
      (DL, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (DL, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
    |];
    [|
      (TW, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (Star, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (TW, "");
    |];
    [|
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
    |];
    [|
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (No, "");
    |];
    [|
      (DL, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (DL, "");
    |];
    [|
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
    |];
    [|
      (No, "");
      (DW, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (TL, "");
      (No, "");
      (No, "");
      (No, "");
      (DW, "");
      (No, "");
    |];
    [|
      (TW, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (No, "");
      (TW, "");
      (No, "");
      (No, "");
      (No, "");
      (DL, "");
      (No, "");
      (No, "");
      (TW, "");
    |];
  |]

(* not mutable for now *)
let multiplier_at (board : t) i j =
  match board.(i).(j) with
  | m, _ -> m

(* not mutable for now *)
let letter_at (board : t) i j =
  match board.(i).(j) with
  | _, c -> c

let gb_el (board : t) row col = board.(row).(col)

let change_el el (s : string) : elt =
  snd el := s;
  el

let set_letter row col t (s : string) = (change_el (gb_el t row col)) s
let el_multiplier (el : elt) = fst el
let el_letter (el : elt) = snd el
let el_at (board : t) i j = board.(i).(j)
let length (board : t) = Array.length board
