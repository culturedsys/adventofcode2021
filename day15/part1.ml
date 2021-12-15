open Core
open Adventofcode2021.Day15

let (width, height, input) = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day15.txt")
  |> parse

let () =
  let graph = build_graph input in
  let first = Map.find_exn graph (0, 0) in
  let last = Map.find_exn graph (width - 1, height - 1) in
  let min = find_min_cost graph first last in
  print_endline @@ string_of_int min