open Core
open Adventofcode2021.Day8

let input = Stdio.In_channel.input_lines (Stdio.In_channel.create "inputs/day8.txt")

let () = List.sum (module Int) ~f: (fun line -> 
    let (_, output) = parse line in 
    List.length (known_digits output)) input 
      |> string_of_int 
      |> print_endline