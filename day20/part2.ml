open Core
open Adventofcode2021.Day20

let (rule, grid) = let lines = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day20.txt") in
  (parse_rule (List.hd_exn lines),
  parse_grid (List.drop lines 2))

let () =
  let result = enhance_n_times 50 rule grid in
  Map.count ~f: Fn.id result |> string_of_int |> print_endline