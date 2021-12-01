open Adventofcode2021.Day1
open Adventofcode2021.Util

let i = open_in "inputs/day1.txt"
let input = List.map int_of_string (input_lines i)

let () = print_endline @@ string_of_int @@ count_increases (sliding_sum input)