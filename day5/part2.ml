open Core
open Adventofcode2021.Day5

let input = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day5.txt")

let result = List.map ~f: parse_line input |>
  plot_all Point.Map.empty |>
  Map.count ~f: (fun c -> c > 1)

let () = print_endline @@ string_of_int result