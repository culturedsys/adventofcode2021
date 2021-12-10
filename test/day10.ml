open Core
open OUnit2
open Adventofcode2021.Day10

let suite1 = "Part 1" >::: [
  "can find illegal char" >:: (fun _ ->
    let result = parse "{([(<{}[<>[]}>{[]{[(<()>" in
    assert_equal (Error '}') result  
  )
]

let suite2 = "Part 2" >::: [
  "correct score" >:: (fun _ ->
    let result = incomplete_score ['['; '('; '{'; '<'] in
    assert_equal 294 result  
  )
]

let () = run_test_tt_main suite1;
  run_test_tt_main suite2