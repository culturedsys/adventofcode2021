open Core
open OUnit2
open Adventofcode2021.Day9

let example = {|2199943210
3987894921
9856789892
8767896789
9899965678|}

let suite = "Day 9" >::: [
  "can find basin" >:: (fun _ ->
    let input = parse (String.split_lines example) in
    assert_equal 9 (basin_size input (0, 9)) ~printer: string_of_int
  )
]

let () = run_test_tt_main suite
