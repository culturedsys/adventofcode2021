open Adventofcode2021.Util
open Adventofcode2021.Day3

let input = input_lines (open_in "inputs/day3.txt") |> List.map bits_of_string

let oxygen_rating = filter most_common_bits input |> int_of_bits

let invert = (List.map Bool.not)

let (<.>) = Base.Fn.compose

let co2_rating = filter (invert <.> most_common_bits) input |> int_of_bits

let () = string_of_int (oxygen_rating * co2_rating) |> print_endline
