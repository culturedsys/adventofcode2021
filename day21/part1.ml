open Core
open Adventofcode2021.Day21

let () =
let scores = scores (grouped_rolls 3 deterministic_rolls) 9 6 in
let (turn, (p1, p2)) = step_to scores 1000 in
((if p1 < 1000 then p1 else p2) * (turn + 1) * 3) |> string_of_int |> print_endline
