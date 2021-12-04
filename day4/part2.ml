open Core
open Adventofcode2021.Day4

let input =  Stdio.In_channel.read_all "inputs/day4.txt"

let (draws, boards) = parse 5 input

let () = last_winning_score draws boards |> string_of_int |> print_endline