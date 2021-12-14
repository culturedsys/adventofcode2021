open Core
open Adventofcode2021.Day14

let input = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day14.txt")

let (seed, rules) = parse input

let () = let result = total_after_steps rules seed 40 |>
  Map.to_alist in
  let min = List.min_elt ~compare: (fun a b -> Int.compare (snd a) (snd b)) result in
  let max = List.max_elt ~compare: (fun a b -> Int.compare (snd a) (snd b)) result in
  match (min, max) with
  | Some(n), Some(x) -> ((snd x) - (snd n)) |> string_of_int |> print_endline
  | _-> failwith "fail"
