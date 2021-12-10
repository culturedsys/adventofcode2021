open Core
open Adventofcode2021.Day10

let input = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day10.txt")

let () =
  let scores = List.concat_map input ~f: (fun line ->
    match parse line with
    | Ok expected -> [incomplete_score expected]
    | Error _ -> []
  ) |> List.sort ~compare: Int.compare in
  List.hd_exn (List.drop scores (List.length scores / 2)) |>
  string_of_int |> print_endline