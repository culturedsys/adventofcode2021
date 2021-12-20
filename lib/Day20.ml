open Core

let value_at_point def s (x, y) = 
  List.concat_map (List.range ~stop: `inclusive (y - 1) (y + 1)) ~f: (fun y' ->
    List.map (List.range ~stop: `inclusive (x - 1) (x + 1)) ~f: (fun x' ->
      Point.Map.find s (x', y') |> Option.value ~default: def   
    )
  ) |>
  List.fold ~init: 0 ~f: (fun acc x -> acc * 2 + (if x then 1 else 0))

let min_of f s = Point.Set.fold (Point.Map.key_set s) ~init: Int.max_value ~f: (fun min x -> if (f x) < min then (f x) else min)  
let max_of f s = Point.Set.fold (Point.Map.key_set s) ~init: Int.min_value ~f: (fun max x -> if (f x) > max then (f x) else max)  

let enhance min_x max_x min_y max_y rule def points = 
  let field = List.concat_map (List.range ~stop: `inclusive (min_y - 1) (max_y + 1)) ~f: (fun y ->
    List.map (List.range ~stop: `inclusive (min_x - 1) (max_x + 1)) ~f: (fun x -> (x, y))) in
  List.fold field ~init: Point.Map.empty ~f: (fun s p ->
    let i = value_at_point def points p in
    if rule.(i) then
      Point.Map.add_exn s ~key: p ~data: true
    else
      Point.Map.add_exn s ~key: p ~data: false
  ) 

let enhance_n_times n rule points =
  let rec go min_x max_x min_y max_y n def points =
    let next = enhance min_x max_x min_y max_y rule def points in
    if n = 1 then next
    else 
      let next_def = if def then rule.(511) else rule.(0) in
      go (min_x - 1) (max_x + 1) (min_y - 1) (max_y + 1) (n - 1) next_def next
  in 
    let min_x = min_of fst points in
    let max_x = max_of fst points in
    let min_y = min_of snd points in
    let max_y = max_of snd points in
    go min_x max_x min_y max_y n false points

let parse_rule s = 
  String.to_array s |> Array.map ~f: (fun c -> Char.equal c '#')

let parse_grid lines = 
  List.concat_mapi lines ~f: (fun y line ->
    List.mapi (String.to_list line) ~f: (fun x c ->
      if Char.equal c '#' then ((x, y), true) else ((x, y), false)
    )
  ) |> Point.Map.of_alist_exn