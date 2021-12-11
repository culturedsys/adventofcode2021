open Core

let rec input_lines ch =
  match Stdio.In_channel.input_line_exn ch with
  | line -> line :: input_lines ch
  | exception End_of_file -> []

let string_of_tuple2 f1 f2 t = String.concat ["("; f1 (fst t); ", ";  f2 (snd t); ")"]

module Grid = struct
  module Point = struct
    include Tuple.Comparable (Int) (Int)
  end

  type 'a grid = {
    cells: 'a Point.Map.t;
    width: int;
    height: int;
  }

  let cell grid row col = Point.Map.find_exn grid.cells (row, col)

  let update grid row col value = {grid with cells = Point.Map.set grid.cells ~key: (row, col) ~data: value}

  let for_all ~f grid = Point.Map.for_all ~f: f grid.cells

  let map ~f grid = { grid with cells = Point.Map.map ~f: f grid.cells }

  let of_list l=
    let width = (List.length (List.hd_exn l)) in
    let height = List.length l in
    { 
      cells = Point.Map.of_alist_exn (List.concat_mapi l ~f: (fun row line ->
          List.mapi line ~f: (fun col value -> ((row, col), value)
        ))); 
      width = width; 
      height = height 
    }

end

