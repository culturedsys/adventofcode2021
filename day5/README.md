# [Day 5: Hydrothermal Venture](https://adventofcode.com/2021/day/5)

Further adventures with [Jane Street's
Core](https://opensource.janestreet.com/core/) library. For Day 4, I needed a
set of `int`'s, which I created by passing the `Int` module when creating a
set. For Day 5, I need a map with `int * int` tuples as a key, and there's no
`int * int` module. Instead, what seems to be the done thing here is to create
a new module and include the functionality needed for the map:

```ocaml
module Point = struct 
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end
```

Including `Tuple.Comparable (Int) (Int)` gives you a `Map` module, so you can
write e.g `Point.Map.empty` to create an empty map.

This map is used to store the count of lines crossing each point:

```ocaml
let signum x = if x < 0 then -1 else if x > 0 then 1 else 0

let points_of_line (x1, y1) (x2, y2) = let dx = signum (x2 - x1) in 
  let dy = signum (y2 - y1) in
  let rec go ps x y = if x = x2 && y = y2 then 
      ((x, y) :: ps) 
    else
      go ((x, y) :: ps) (x + dx) (y + dy) in 
  go [] x1 y1

let plot m s e = 
    let points = points_of_line s e in
    let add_point = function 
      | Some(count) -> count + 1
      | None -> 1 in
    List.fold ~f: (fun m p -> Map.update ~f: add_point m p) ~init: m points
```