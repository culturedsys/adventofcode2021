open Core
open Adventofcode2021.Day18

let input = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day18.txt")

let () =
  let result = max input in 
  string_of_int result |> print_endline 
