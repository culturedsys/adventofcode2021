open Core
open Adventofcode2021.Day9

let input = parse (Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day9.txt"))

let () = List.sum (module Int) ~f: (fun (row, col) -> 1 + (cell input row col)) (low_points input) |> 
  string_of_int |> 
  print_endline