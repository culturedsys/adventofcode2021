open Adventofcode2021.Day3
open Adventofcode2021.Util
open Base.Int

let input = input_lines (open_in "inputs/day3.txt") |> parse_input
let gamma_bits = most_common_bits input 

let gamma = gamma_bits |> int_of_bits
let epsilon = invert gamma_bits |> int_of_bits

let () = string_of_int (gamma * epsilon) |> print_endline