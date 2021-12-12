open Core
open OUnit2
open Adventofcode2021.Day12

let suite1 = "Part 1" >::: [
  "correct answer for example" >:: (fun _ ->
    let connections = [("start", "A"); ("start", "b"); ("A", "c"); ("A", "b"); ("b", "d"); ("A", "end"); ("b", "end")] in
    let result = find_paths connections |> List.length in
    assert_equal 10 result
  )
]

let () = run_test_tt_main suite1