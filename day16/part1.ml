open Core
open Adventofcode2021.Day16

let input = Stdio.In_channel.input_line_exn (Stdio.In_channel.create "inputs/day16.txt")
  |> binary_of_hex

let () = let (packet, _, _) = parse_packet input in
 sum_versions packet |> string_of_int |> print_endline