open Core
open Adventofcode2021.Day13

let (point_list, folds) = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day13.txt") |> 
  parse

let () =
  let points = Set.of_list (module Adventofcode2021.Point) point_list in
  let result = List.fold ~f: (fun acc (axis, n) -> flip_all axis n acc) ~init: points folds in
  let edge compare extract init = Set.fold ~f: (fun m point -> 
      compare (extract point) m) ~init: init result in
  let min_x = edge Int.min fst Int.max_value in
  let max_x = edge Int.max fst Int.min_value in
  let min_y = edge Int.min snd Int.max_value in
  let max_y = edge Int.max snd Int.min_value in
  print_endline @@ String.concat ~sep: ", " [string_of_int min_x; string_of_int max_x];
  print_endline @@ String.concat ~sep: ", " [string_of_int min_y; string_of_int max_y];
  List.iter (List.range ~stop: `inclusive min_y max_y) ~f: (fun y ->
    List.iter (List.range ~stop: `inclusive min_x max_x) ~f: (fun x -> 
      print_string (if Set.mem result (x, y) then "#" else ".")
      ); 
    print_endline ""
  )