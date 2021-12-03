open Adventofcode2021.Day3
open Adventofcode2021.Util
open Base.Int

let input = input_lines (open_in "inputs/day3.txt")

let gamma = calculate_totals input
let epsilon = (2 ** (String.length @@ List.hd input)  - 1)  - gamma

let () = print_endline @@ string_of_int @@ gamma * epsilon