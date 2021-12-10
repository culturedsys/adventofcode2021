open Core
open Adventofcode2021.Day10

let input = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day10.txt")

let () =
  List.sum (module Int) input ~f: (fun line ->
    match parse line with
    | Ok _ -> 0
    | Error c -> illegal_score c
  ) |> string_of_int |> print_endline