open Core
open Adventofcode2021.Day13

let (point_list, folds) = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day13.txt") |> parse

let () =
  let points = Set.of_list (module Adventofcode2021.Point) point_list in
  let (axis, n) = List.hd_exn folds in
  flip_all axis n points |> Set.length |> string_of_int |> print_endline