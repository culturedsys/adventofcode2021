open Core
open OUnit2
open Adventofcode2021.Day6

let example = histogram [3; 4; 3; 1; 2]

let suite = "Day 6" >::: [
  "histogram" >:: (fun _ ->
    let result = histogram [3; 4; 3; 1; 2] in
    assert_bool "Not equal" @@
      Map.equal (Int.equal) (Int.Map.of_alist_exn [(3, 2); (4, 1); (1, 1); (2, 1)]) result
  );

  "generation" >:: (fun _ ->
    let result = generation (histogram [2; 3; 2; 0; 1]) in
    assert_bool "Not equal" @@
      Map.equal (Int.equal) (Int.Map.of_alist_exn [(1, 2); (2, 1); (6, 1); (0, 1); (8, 1)]) result
  );


  "correct answer for short example" >:: (fun _ ->
    let result = Fn.apply_n_times ~n: 18 generation example  in
    assert_equal 26 (total result) ~printer: string_of_int
  );

  "correct answer for example" >:: (fun _ ->
    let result = Fn.apply_n_times ~n: 80 generation example  in
    assert_equal 5934 (total result) ~printer: string_of_int
  )
]

let () = run_test_tt_main suite