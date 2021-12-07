open Core
open OUnit2
open Adventofcode2021.Day7

let example = [16.;1.;2.;0.;4.;2.;7.;1.;2.;14.]

let suite1 = "Part 1" >::: [
  "correct answer for example" >:: (fun _ ->
    let result = cost example in
    assert_equal 37. result
  )
]

let suite2 = "Part 2" >::: [
  "correct answer for example" >:: (fun _ ->
    let result = min_cost example in
    assert_equal 168. result ~printer: string_of_float
  )
]


let () = 
  run_test_tt_main suite1;
  run_test_tt_main suite2