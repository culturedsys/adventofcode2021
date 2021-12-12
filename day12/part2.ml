open Core
open Adventofcode2021.Day12

let input = parse (Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day12.txt"))

let () =
  find_paths `allow_repeat input |> List.length |> string_of_int |> print_endline