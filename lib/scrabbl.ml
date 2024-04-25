let dict = Arg.read_arg "data/dictionary.txt"

module StringSet = Set.Make (struct
  type t = string

  let compare a b =
    compare (String.lowercase_ascii a) (String.lowercase_ascii b)
end)

let dict = StringSet.of_list (Array.to_list dict)
let valid_word str = StringSet.mem str dict
let grade_word w = failwith "TODO"
