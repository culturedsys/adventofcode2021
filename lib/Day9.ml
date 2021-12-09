open Core

module Point = struct
  include Tuple.Comparable (Int) (Int)
end

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

let neighbours grid row col = List.concat [
  if row = 0 && col = 0 then [] 
  else if row = 0 then [(row, col - 1)] 
  else if col = 0 then [(row - 1, col)] 
  else [(row - 1, col); (row, col - 1)];
  if row = (grid.height - 1) && col = (grid.width - 1) then [] 
  else if row = (grid.height - 1) then [(row, col + 1)]
  else if col = (grid.width - 1) then [(row + 1, col)]
  else [(row + 1, col); (row, col + 1)]
] 

let is_low_point grid row col =
  List.for_all (neighbours grid row col) ~f: (fun (nrow, ncol) -> 
      (cell grid row col) < (cell grid nrow ncol))

let low_points grid = List.concat_map (List.range 0 grid.height) ~f: (fun row ->
    List.map (List.range 0 grid.width) ~f: (fun col ->
      if is_low_point grid row col then Some(row, col) else None  
    )
  ) |> List.filter_opt

let basin_size grid (row, col) = 
  let is_higher_than h (row, col) = let nh = (cell grid row col) in nh <> 9 && nh > h in
  let rec go (row, col) = 
    let height = cell grid row col in
    let ns = neighbours grid row col |> List.filter ~f: (is_higher_than height) in
    (row, col) :: (List.concat_map ~f: go ns) in
  go (row, col) |> List.dedup_and_sort ~compare: Point.compare |> List.length
