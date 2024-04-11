type multiplier =
  | TW
  | DW
  | TL
  | DL
  | Star
  | No

type elt = multiplier * string ref
type t = elt array array

let empty : t =
  [|
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
  |]

let init : t =
  [|
    [|
      (TW, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (TW, ref "");
    |];
    [|
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (TW, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (Star, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (TW, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
    |];
    [|
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
    |];
    [|
      (No, ref "");
      (DW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DW, ref "");
      (No, ref "");
    |];
    [|
      (TW, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (TW, ref "");
      (No, ref "");
      (No, ref "");
      (No, ref "");
      (DL, ref "");
      (No, ref "");
      (No, ref "");
      (TW, ref "");
    |];
  |]

(* not mutable for now *)
let multiplier_at (board : t) i j =
  match board.(i).(j) with
  | m, _ -> m

(* not mutable for now *)
let letter_at (board : t) i j =
  match board.(i).(j) with
  | _, c -> !c

let gb_el (board : t) row col = board.(row).(col)
let change_el el (s : string) = snd el := s
let set_letter row col t (s : string) = (change_el (gb_el t row col)) s
let el_multiplier (el : elt) = fst el
let el_letter (el : elt) = !(snd el)
let el_at (board : t) i j = board.(i).(j)
let length (board : t) = Array.length board
