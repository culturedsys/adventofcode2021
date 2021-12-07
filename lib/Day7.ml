open Core

let median xs = 
  let sorted = List.sort xs ~compare: Float.compare in
  let l = List.length sorted in
  let result = if (l / 2) * 2 = l then
    let [@warning "-8"] [a; b] = List.take (List.drop sorted (l / 2 - 1)) 2 in
    (a +. b) /. 2.
  else
    List.hd_exn (List.drop sorted (l / 2)) in
  result

let cost xs =
  let target = median xs in
  List.sum (module Float) ~f: (fun x -> Float.abs (x -. target)) xs

let min_cost xs =
  let min = List.min_elt ~compare: Float.compare xs |> Option.value ~default: 0. |> int_of_float in
  let max = List.max_elt ~compare: Float.compare xs |> Option.value ~default: 0. |> int_of_float in
  let cost_one a b = let n = Float.abs (a -. b) in (n *. (n +. 1.)) /. 2. in
  let cost target = List.sum (module Float) ~f: (cost_one target) xs in
  let costs = List.range ~stop: `inclusive min max |> List.map ~f: (fun target -> cost (float_of_int target)) in
  List.min_elt ~compare: Float.compare costs |> Option.value ~default: 0.