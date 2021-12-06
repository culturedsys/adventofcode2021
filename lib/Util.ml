open Core

let rec input_lines ch =
  match Stdio.In_channel.input_line_exn ch with
  | line -> line :: input_lines ch
  | exception End_of_file -> []

let string_of_tuple2 f1 f2 t = String.concat ["("; f1 (fst t); ", ";  f2 (snd t); ")"]