(* Check if a file exists (helper function for find_project_root)*)
let file_exists path =
  try
    let _ = Unix.stat path in
    true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

(* Find the project root by looking for the dune-project file *)
let rec find_project_root_aux current_dir =
  let potential_root = Filename.concat current_dir "dune-project" in
  if file_exists potential_root then current_dir
  else
    let parent_dir = Filename.dirname current_dir in
    if String.equal current_dir parent_dir then
      failwith "dune-project file not found in any parent directories"
    else find_project_root_aux parent_dir

let project_root () = find_project_root_aux (Sys.getcwd ())

type multiplier =
  | TW
  | DW
  | TL
  | DL
  | Star
  | No

type elt = multiplier * string ref * bool ref * bool ref
type t = elt array array

let match_mult = function
  | "DL" -> DL
  | "TL" -> TL
  | "DW" -> DW
  | "TW" -> TW
  | "Star" -> Star
  | _ -> No

let new_elt el = (match_mult el, ref "", ref false, ref false)
let board = Csv.to_array (Csv.load (project_root () ^ "/data/board.csv"))

let empty : t =
  Array.init 15 (fun _ ->
      Array.init 15 (fun _ -> (No, ref "", ref false, ref false)))

let init : t =
  Array.init 15 (fun x -> Array.init 15 (fun y -> new_elt board.(x).(y)))

(* not mutable for now *)
let multiplier_at (board : t) i j =
  match board.(i).(j) with
  | m, _, _, _ -> m

let letter_at (board : t) i j =
  match board.(i).(j) with
  | _, c, _, _ -> !c

let played_at (board : t) i j =
  match board.(i).(j) with
  | _, _, b, _ -> !b

let was_blank_at (board : t) i j =
  match board.(i).(j) with
  | _, _, _, b -> !b

let gb_el (board : t) row col = board.(row).(col)

let change_el el (s : string) =
  let _, b, _, _ = el in
  b := s

let change_blank el boo =
  let _, _, _, b = el in
  b := boo

let set_letter row col t (s : string) = (change_el (gb_el t row col)) s
let set_was_blank row col t b = change_blank (gb_el t row col) b

let play_letter row col t =
  let _, _, c, _ = gb_el t row col in
  c := true

let el_multiplier (el : elt) =
  match el with
  | a, _, _, _ -> a

let el_letter (el : elt) =
  match el with
  | _, b, _, _ -> !b

let el_played (el : elt) =
  match el with
  | _, _, c, _ -> !c

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
