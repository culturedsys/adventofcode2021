open Core
open Adventofcode2021.Day9

let input = parse (Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day9.txt"))

let () = 
  let lows = low_points input in
  let basins = List.map ~f: (basin_size input) lows in
  match List.sort ~compare: (fun a b -> Int.compare b a) basins with
  | a :: b :: c :: _ -> print_endline @@ string_of_int (a * b * c)
  | _ -> failwith "Less than three basins"
   