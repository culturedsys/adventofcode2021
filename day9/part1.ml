open Core
open Adventofcode2021.Day9

let input = parse (Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day9.txt"))

let () = List.sum (module Int) ~f: (fun height -> 1 + height) (low_points input) |> 
  string_of_int |> 
  prerr_endline