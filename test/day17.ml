open Core
open OUnit2
open Adventofcode2021.Day17

let suite = "Day 17" >::: [
  "correct for hitting trajectory" >:: (fun _ ->
    assert_bool "Should intersect" (intersects 20 30 (-10) (-5) (6, 0))  
  );
  "correct for missing trajectory" >:: (fun _ ->
    assert_bool "Should not intersect" (not (intersects 20 30 (-10) (-5) (17, -4)))  
  );
  "correct count intersecting trajectories" >:: (fun _ ->
    assert_equal 112 (count_intersecting_trajectories 20 30 (-10) (-5))  ~printer: string_of_int
  )
]

let () = run_test_tt_main suite