let bits_of_string s = Base.String.to_list s
  |> List.map (fun b -> if b == '0' then false else true)

let int_of_bits b = 
  List.fold_left (fun acc b -> acc * 2 + (if b then 1 else 0)) 0 b

let parse_input = List.map bits_of_string

let count_bits s = 
  let zeros = List.map (Base.Fn.const 0) (List.hd s) in
  let add_bits_to_count count bits = List.map2 (fun c b -> c + (if b then 1 else 0)) count bits in 
  List.fold_left add_bits_to_count zeros s

(** Take a list of binary numbers, and return the binary number containing, in each position, the bit that is most common for that
  position in the  list. If 1 and 0 are equally common in a position, 1 is chosen *)
let most_common_bits s = let l = List.length s in
  count_bits s |>
  List.map (fun count -> count >= (l - count))

let invert = (List.map Bool.not)

let filter f haystack =
  let rec go remaining index = match remaining with
    | [answer] -> answer
    | _ ->     
      let needle = f remaining in 
      let match_index l = (List.nth l index) == (List.nth needle index )
      in go (List.filter match_index remaining) (index + 1 ) 
  in go haystack 0