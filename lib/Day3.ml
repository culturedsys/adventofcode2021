
let increment_count count bits = List.map2 (fun c b -> c + (if b then 1 else 0)) count bits

let count_bits s = let zeros = (List.init (List.length (List.hd s)) (Base.Fn.const 0)) in
    List.fold_left increment_count zeros s

let bits_of_string s = List.init (String.length s) (String.get s)
  |> List.map (fun b -> if b == '0' then false else true)

let int_of_bits b = 
  List.fold_left (fun acc b -> acc * 2 + (if b then 1 else 0)) 0 b

let calculate_totals ns =
  List.map bits_of_string ns |>
  count_bits |>
  List.map (fun x -> if x > (List.length ns) / 2 then true else false) |>
  int_of_bits

let most_common_bits s = let l = List.length s in
  count_bits s |>
  List.map (fun count -> count >= (l - count))

let filter f haystack =
  let rec go remaining index = match remaining with
    | [answer] -> answer
    | _ ->     
      let needle = f remaining in 
      let match_index l = (List.nth l index) == (List.nth needle index )
      in go (List.filter match_index remaining) (index + 1 ) 
  in go haystack 0