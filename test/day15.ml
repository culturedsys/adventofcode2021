open Core
open Adventofcode2021.Day15
open OUnit2

let suite1 = "Part 1" >::: [
  "trivial example" >:: (fun _ ->
    let input = build_graph [
      { x = 0; y = 0; cost = 0 };
      { x = 1; y = 0; cost = 1 };
      { x = 0; y = 1; cost = 2 };
      { x = 1; y = 1; cost = 3 };
    ] in
    let first = Map.find_exn input (0, 0) in
    let last = Map.find_exn input (1, 1) in
    let result = find_min_cost input first last in
    assert_equal 4 result
  )
]

let () = run_test_tt_main suite1