open Core
open Adventofcode2021.Day7

let input = Stdio.In_channel.input_line_exn (Stdio.In_channel.create "inputs/day7.txt") |> 
  String.split ~on: ',' |>
  List.map ~f: float_of_string

let () = cost input |> string_of_float |> print_endline