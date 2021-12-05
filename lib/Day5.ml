open Core

module Point = struct 
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end

let line_pattern = Re2.create_exn "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"

let parse_line line = match Re2.find_submatches line_pattern line with
  | Ok([|_; Some(x1); Some(y1); Some(x2); Some(y2)|]) -> 
      ((int_of_string x1, int_of_string y1), (int_of_string x2, int_of_string y2))
  | _ -> failwith "Parse error"

let signum x = if x < 0 then -1 else if x > 0 then 1 else 0

let points_of_line (x1, y1) (x2, y2) = let dx = signum (x2 - x1) in 
  let dy = signum (y2 - y1) in
  let rec go ps x y = if x = x2 && y = y2 then 
      ((x, y) :: ps) 
    else
      go ((x, y) :: ps) (x + dx) (y + dy) in 
  go [] x1 y1

let plot m s e = 
    let points = points_of_line s e in
    let add_point = function 
      | Some(count) -> count + 1
      | None -> 1 in
    List.fold ~f: (fun m p -> Map.update ~f: add_point m p) ~init: m points

let plot_all m ls = 
  List.fold ~f: (fun m (s, e) -> plot m s e) ~init: m ls

let remove_diagonals ls = List.filter ~f: (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2) ls