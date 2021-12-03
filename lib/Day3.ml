
let add_count count bits = List.map2 (fun c b -> c + (if b then 1 else 0)) count bits

let count_bits s = 
    List.fold_left add_count (List.init (List.length (List.hd s)) (fun _ -> 0)) s

let bits_of_string s = List.init (String.length s) (String.get s)
  |> List.map (fun b -> if b == '0' then false else true)

let int_of_bits b = 
  List.fold_left (fun acc b -> acc * 2 + b) 0 b

let calculate_totals ns =
  List.map bits_of_string ns |>
  count_bits |>
  List.map (fun x -> if x > (List.length ns) / 2 then 1 else 0) |>
  int_of_bits