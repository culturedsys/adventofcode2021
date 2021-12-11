open Core
open Util

let parse lines = List.map lines ~f: (fun line -> 
    String.to_list line |> 
    List.map ~f: (fun c -> int_of_string (Char.to_string c))) |>
  Grid.of_list

let increment (grid: int Grid.grid) = Grid.map ~f: ((+) 1) grid

let neighbours (grid: int Grid.grid) row col = List.filter ~f: (fun (row, col) -> 
  row >= 0 && row < grid.height && col >= 0 && col < grid.width) [
    (row - 1, col - 1);
    (row - 1, col);
    (row - 1, col + 1);
    (row, col - 1);
    (row, col + 1);
    (row + 1, col - 1);
    (row + 1, col);
    (row + 1, col + 1)
  ] 

let rec update_neighbours grid row col flashes =
  List.fold (neighbours grid row col) ~init: (grid, flashes) ~f: (fun (grid, flashes) (nr, nc) ->
    let cell = Grid.cell grid nr nc in
    if cell = 9 then
      let (updated, new_flashes) = flash grid nr nc in
      (updated, flashes + new_flashes)
    else if cell <> 0 then
      (Grid.update grid nr nc (cell + 1), flashes)
    else
      (grid, flashes)
  ) 
and flash grid row col = 
  let updated = Grid.update grid row col 0 in
  update_neighbours updated row col 1

let flashes initial_grid = 
  let rec go (grid: int Grid.grid) flashes row col =
    if col = grid.width && row = (grid.height - 1) then
      (grid, flashes)
    else if col = grid.width then
      go grid flashes (row + 1) 0
    else if Grid.cell grid row col > 9 then
      let (updated, new_flashes) = flash grid row col in
      go updated (flashes + new_flashes) row (col + 1) 
    else
      go grid flashes row (col + 1)  
    in

  go initial_grid 0 0 0

let step grid = 
  increment grid |>
  flashes

let counting_step (grid, initial_flashes) =
  let (updated, new_flashes) = step grid in
  (updated, new_flashes + initial_flashes)

let first_all_flash initial_grid = 
  let rec go (grid: int Grid.grid) i =
    if Grid.for_all ~f: (fun value -> value = 0) grid then
      i
    else 
      let (next, _) = step grid in
      go next (i + 1) in
  go initial_grid 0