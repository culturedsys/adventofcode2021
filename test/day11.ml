open Core
open OUnit2
open Adventofcode2021.Day11

let small_example = {|11111
19991
19191
19991
11111|}

let full_example = {|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526|}

let suite1 = "Part 1" >::: [
  "one iteration" >:: (fun _ ->
    let example = parse @@ String.split_lines small_example in
    let result = step example in
    assert_equal 9 (snd result) ~printer: string_of_int
  );
  "100 iterations" >:: (fun _ ->
    let example = parse @@ String.split_lines full_example in
    let (_, flashes) = Fn.apply_n_times ~n: 100 counting_step (example, 0) in
    assert_equal 1656 flashes ~printer: string_of_int
  )
]

let suite2 = "Part 2" >::: [
  "correct value for example" >:: (fun _ ->
    let example = parse @@ String.split_lines full_example in
    let result = first_all_flash example in
    assert_equal 195 result
  )
]

let () = 
  run_test_tt_main suite1;
  run_test_tt_main suite2