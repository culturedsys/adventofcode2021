open OUnit2
open Adventofcode2021.Day3

let example = [
  "00100";
  "11110";
  "10110";
  "10111";
  "10101";
  "01111";
  "00111";
  "11100";
  "10000";
  "11001";
  "00010";
  "01010";
]

let suite1 = "Day3 part 1" >::: [
  "count bits" >:: (fun _ -> 
    assert_equal [3; 1; 2] 
      (count_bits [[true; false; true]; [true; true; true]; [true; false; false]])
  );
  "correct answer for example" >:: (fun _ ->
    assert_equal 22 (calculate_totals example)
  )
]

let () = 
  run_test_tt_main suite1