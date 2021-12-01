open OUnit2
open Adventofcode2021.Day1
  
let data = [
  199;
  200;
  208;
  210;
  200;
  207;  
  240;
  269;
  260;
  263
]

let suite1 = "Day1 part 1" >::: [
  "correct result for test input" >:: (fun _ -> let result = count_increases data in 
    assert_equal 7 result ~printer: string_of_int
  );
]

let suite2 = "Day2 part 2" >::: [
  "sliding sums gives correct sum" >:: (fun _ -> let result = sliding_sum data in
    assert_equal [
      607;
      618;
      618;
      617;
      647;
      716;
      769;
      792
    ] result ~printer: (fun l -> String.concat "," (List.map string_of_int l))
  );
  "count_increses of sliding_sum for test input" >:: (fun _ -> 
    let result = count_increases (sliding_sum data) in 
      assert_equal result 5)
]

let () = 
  run_test_tt_main suite1;
  run_test_tt_main suite2