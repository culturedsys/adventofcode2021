open Adventofcode2021.Day2
open Adventofcode2021.Util

let input = input_lines (open_in "inputs/day2.txt")

let commands = List.map parse input

let result = run {horizontal = 0 ; depth = 0} commands

let () = print_endline @@ string_of_int (result.horizontal * result.depth)