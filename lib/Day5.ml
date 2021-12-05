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

let points_of_line (x1, y1) (x2, y2) = let (start, stop, add_constant) = if x1 = x2 then
    (y1, y2, fun y -> (x1, y))
  else if y1 = y2 then
    (x1, x2, fun x -> (x, y1))
  else
    failwith "Cannot handle diagonal lines" in
  List.range ~stride: (compare stop start) ~stop: `inclusive start stop |> List.map ~f: add_constant

let plot m s e = 
    let points = points_of_line s e in
    let add_point = function 
      | Some(count) -> count + 1
      | None -> 1 in
    List.fold ~f: (fun m p -> Map.update ~f: add_point m p) ~init: m points

let plot_all m ls = 
  List.fold ~f: (fun m (s, e) -> plot m s e) ~init: m ls

let remove_diagonals ls = List.filter ~f: (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2) ls