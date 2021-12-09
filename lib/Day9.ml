open Core

type grid = {
  cells: int array;
  width: int;
  height: int;
}

let cell grid row col = grid.cells.(row * grid.width + col)

let parse lines = let width = String.length (List.hd_exn lines) in
  let height = List.length lines in
  let cells = Array.of_list @@ List.concat_map lines  ~f: (fun line -> 
    String.to_list line 
      |> List.map ~f: (fun c -> (Char.to_int c) - (Char.to_int '0'))) in
  { cells = cells; width = width; height = height }

let is_low_point grid row col = let neighbours = List.concat [
    if row = 0 && col = 0 then [] 
    else if row = 0 then [cell grid row (col - 1)] 
    else if col = 0 then [cell grid (row - 1) col] 
    else [cell grid (row - 1) col; cell grid row (col - 1)];
    if row = (grid.height - 1) && col = (grid.width - 1) then [] 
    else if row = (grid.height - 1) then [cell grid row (col + 1)]
    else if col = (grid.width - 1) then [cell grid (row + 1) col]
    else [cell grid (row + 1) col; cell grid row (col + 1)]
  ] in
  List.for_all neighbours ~f: (fun neighbour -> (cell grid row col) < neighbour)

let low_points grid = List.concat_map (List.range 0 grid.height) ~f: (fun row ->
    List.map (List.range 0 grid.width) ~f: (fun col ->
      if is_low_point grid row col then Some(cell grid row col) else None  
    )
  ) |> List.filter_opt