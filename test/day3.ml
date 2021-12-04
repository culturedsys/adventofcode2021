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
    assert_equal 22 (example |> parse_input |> most_common_bits |> int_of_bits)
  )
]

let suite2 = "Day3 part 2" >::: [
  "most common bits" >:: (fun _ ->
    assert_equal [true ; false ; true] 
      (most_common_bits [[true; false; true]; [true; true; true]; [true; false; false]])
      ~printer: (Base.Fn.compose (String.concat ", ") (List.map string_of_bool))  
  );
  "correct answer for example" >:: (fun _ ->
    assert_equal 23 (example |> parse_input |> filter most_common_bits |> int_of_bits) ~printer: string_of_int  
  )
 ]

let () = 
  run_test_tt_main suite1;
  run_test_tt_main suite2