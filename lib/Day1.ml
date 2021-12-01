let count_increases l = let first = List.hd l and rest = List.tl l in 
  fst @@ List.fold_left (fun (increases, prev) next -> 
      if next > prev then (increases + 1, next) else (increases, next)) 
    (0, first) rest

let sliding_sum l =
    let rec go acc l = match l with
      | (a :: b :: c :: xs) -> go ((a + b + c ) :: acc) (b :: c :: xs)
      | _ -> acc
    in
      List.rev (go [] l)