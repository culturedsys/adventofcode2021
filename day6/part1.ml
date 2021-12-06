open Core
open Adventofcode2021.Day6

let input = Stdio.In_channel.input_line_exn (Stdio.In_channel.create "inputs/day6.txt") |>
  String.split ~on: ',' |> List.map ~f: int_of_string

let () = Fn.apply_n_times ~n: 80 generation (histogram input) |> total |> string_of_int |> print_endline