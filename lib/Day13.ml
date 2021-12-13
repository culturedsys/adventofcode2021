open Core

type axis = [`x | `y]
let flip axis n (x, y) = match axis with
  | `x -> if x > n then (2 * n - x, y) else (x, y)
  | `y -> if y > n then (x, 2 * n - y) else (x, y)

let flip_all axis n points = Set.map (module Point) ~f: (flip axis n) points

let parse_point line =
  match String.split ~on: ',' line with
  | x :: y :: _ -> (int_of_string x, int_of_string y)
  | _ -> failwith "Parse error"

let fold_pattern = Re2.create_exn "fold along ([x|y])=([0-9]+)"

let parse_fold line = 
  match Re2.find_submatches fold_pattern line with
  | Ok([|_; Some("x"); Some(n)|]) -> (`x, int_of_string n)
  | Ok([|_; Some("y"); Some(n)|]) -> (`y, int_of_string n)
  | _ -> failwith "Parse error"

let parse lines = 
  let (points, folds) = List.split_while ~f: (fun l -> (String.length l) > 0) lines in
  (List.map ~f: parse_point points), (List.map ~f: parse_fold (List.tl_exn folds))