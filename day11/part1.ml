open Core
open Adventofcode2021.Day11

let input = Stdio.In_channel.create "inputs/day11.txt" |> Stdio.In_channel.input_lines |> parse

let () = 
  let (_, flashes) = Fn.apply_n_times ~n: 100 counting_step (input, 0) in
  print_endline @@ string_of_int flashes