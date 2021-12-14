open OUnit2
open Adventofcode2021.Day14

let suite1 = "Part 1" >::: [
  "correct answer for one iteration" >:: (fun _ ->
    let rules = Pair.Map.of_alist_exn [(('N', 'N'),  'C');
      (('N', 'C'), 'B');
      (('C', 'B'), 'H')
    ] in
    let result = step rules ['N'; 'N'; 'C'; 'B'] in
    assert_equal ['N'; 'C'; 'N'; 'B'; 'C'; 'H'; 'B'] result
  )
]

let () = run_test_tt_main suite1