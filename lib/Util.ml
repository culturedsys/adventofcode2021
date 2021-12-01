let rec input_lines ch =
  match input_line ch with
  | line -> line :: input_lines ch
  | exception End_of_file -> []
