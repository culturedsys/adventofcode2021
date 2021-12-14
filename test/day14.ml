open Core
open OUnit2
open Adventofcode2021.Day14

let rules = Pair.Map.of_alist_exn [(('N', 'N'),  'C');
(('N', 'C'), 'B');
(('C', 'B'), 'H')
]

let suite1 = "Part 1" >::: [
  "correct answer for one iteration" >:: (fun _ ->
    let result = step rules ['N'; 'N'; 'C'; 'B'] in
    assert_equal ['N'; 'C'; 'N'; 'B'; 'C'; 'H'; 'B'] result
  )
]

let suite2 = "Part 2" >::: [
  "correct totals for one iteration" >:: (fun _ ->
    let result = total_after_steps rules ['N'; 'N'; 'C'; 'B'] 1 in
    assert_equal ~cmp: (Map.equal Int.equal) (Map.of_alist_exn (module Char) [('N', 2); ('C', 2); ('B', 2); ('H', 1)]) result
  ) 
]

let () = run_test_tt_main suite1;
    run_test_tt_main suite2