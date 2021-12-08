open Core
open Adventofcode2021.Day8

let input = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day8.txt")

let calculate inputs output =
  let mapping = List.map ~f: digits inputs |> assignments in
  pattern_to_int (active_segments (digits output) mapping)

let () = List.sum (module Int) ~f: (fun line -> 
    let (input, output) = parse line in 
    (List.map ~f: (calculate input) output) |>
    (List.fold ~f: (fun acc n -> acc * 10 + n) ~init: 0) ) input 
      |> string_of_int 
      |> print_endline