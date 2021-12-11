open Core
open Adventofcode2021.Day11

let input = Stdio.In_channel.create "inputs/day11.txt" |> Stdio.In_channel.input_lines |> parse

let () = 
  print_endline @@ string_of_int @@ first_all_flash input