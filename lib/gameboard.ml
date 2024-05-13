type multiplier =
  | TW
  | DW
  | TL
  | DL
  | Star
  | No

type elt = multiplier * string ref * bool ref
type t = elt array array

let match_mult = function
  | "DL" -> DL
  | "TL" -> TL
  | "DW" -> DW
  | "TW" -> TW
  | "Star" -> Star
  | _ -> No

let new_elt el = (match_mult el, ref "", ref false)
let board = Csv.to_array (Csv.load "data/board.csv")

let empty : t =
  Array.init 15 (fun _ -> Array.init 15 (fun _ -> (No, ref "", ref false)))

let init : t =
  Array.init 15 (fun x -> Array.init 15 (fun y -> new_elt board.(x).(y)))

(* not mutable for now *)
let multiplier_at (board : t) i j =
  match board.(i).(j) with
  | m, _, _ -> m

(* not mutable for now *)
let letter_at (board : t) i j =
  match board.(i).(j) with
  | _, c, _ -> !c

let played_at (board : t) i j =
  match board.(i).(j) with
  | _, _, b -> !b

let gb_el (board : t) row col = board.(row).(col)

let change_el el (s : string) =
  let _, b, _ = el in
  b := s

let set_letter row col t (s : string) = (change_el (gb_el t row col)) s

let play_letter row col t =
  let _, _, c = gb_el t row col in
  c := true

let el_multiplier (el : elt) =
  match el with
  | a, _, _ -> a

let el_letter (el : elt) =
  match el with
  | _, b, _ -> !b

let el_played (el : elt) =
  match el with
  | _, _, c -> !c

let el_at (board : t) i j = board.(i).(j)
let length (board : t) = Array.length board

let is_empty (board : t) =
  let empty = ref true in
  for i = 0 to Array.length board - 1 do
    for j = 0 to Array.length (Array.get board 0) - 1 do
      if played_at board i j then empty := false
    done
  done;
  !empty
