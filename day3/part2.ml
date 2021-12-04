open Adventofcode2021.Util
open Adventofcode2021.Day3

let input = input_lines (open_in "inputs/day3.txt") |> parse_input

let oxygen_rating = filter most_common_bits input |> int_of_bits

let co2_rating = filter (Base.Fn.compose invert most_common_bits) input |> int_of_bits

let () = string_of_int (oxygen_rating * co2_rating) |> print_endline
