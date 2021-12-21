open Core
open Adventofcode2021.Day21

let (p1wins, p2wins) = wins_starting_from `player1 0 0 9 6

let () =
  print_endline @@ string_of_int @@ max p1wins p2wins