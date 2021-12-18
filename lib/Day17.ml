open Core

let intersects min_x max_x min_y max_y trajectory =
  let rec go x y dx dy =
    if x >= min_x && x <= max_x && y >= min_y && y <= max_y then
      true
    else if x > max_x || y < min_y then
      false
    else
      go (x + dx) (y + dy) (max 0 (dx - 1)) (dy - 1)
    in
  go 0 0 (fst trajectory) (snd trajectory)

let count_intersecting_trajectories min_x max_x min_y max_y =
  let min_dx = int_of_float (sqrt (float_of_int (min_x * 2))) in
  let max_dx = max_x in
  let min_dy = min_y in
  let max_dy = (abs min_y) - 1 in
  let intersecting = List.range min_dx max_dx ~stop: `inclusive |> List.concat_map ~f: (fun x ->
    List.range min_dy max_dy ~stop: `inclusive |> List.map ~f: (fun y -> (x, y))
  ) |> 
  List.filter ~f: (intersects min_x max_x min_y max_y) in
  List.length intersecting