open OUnit2
open Adventofcode2021.Day2

let initial = {horizontal = 0; depth = 0}

let data = [
  Forward 5;
  Down 5;
  Forward 8;
  Up 3;
  Down 8;
  Forward 2;
]

let suite1 = "Day2 part 1" >::: [
  "correctly parses forward" >:: (fun _ -> 
    assert_equal (Forward 5) (parse "forward 5"); 
  );
  "correctly parses down" >:: (fun _ -> 
    assert_equal (Down 5) (parse "down 5"); 
  );
  "correctly parses up" >:: (fun _ -> 
    assert_equal (Up 5) (parse "up 5") 
  );

  "correctly evaluates Forward" >:: (fun _ ->
    assert_equal {horizontal = 5; depth = 0} (eval initial (Forward 5))
  );
  "correctly evaluates Down" >:: (fun _ ->
    assert_equal {horizontal = 0; depth = 5} (eval initial (Down 5))
  );
  "correctly evaluates Up" >:: (fun _ ->
    assert_equal {horizontal = 0; depth = -5} (eval initial (Up 5) )
  );

  "correctly evaluates series of commands" >:: (fun _ ->
    assert_equal {horizontal = 15; depth = 10} (run initial data)
  )
]

let suite2 = "Day2 part 2" >::: [
  "correctly evaluates forward with aim" >:: (fun _ ->
    assert_equal {horizontal = 5; depth = 10; aim = 2} 
      (eval_with_aim {horizontal = 0 ; depth = 0 ; aim = 2} (Forward 5)) ~printer: string_of_env
  );

  "correctly evaluates down with aim" >:: (fun _ ->
    assert_equal {horizontal = 5; depth = 10; aim = 7} 
      (eval_with_aim {horizontal = 5 ; depth = 10 ; aim = 2} (Down 5)) ~printer: string_of_env
  );

  "correctly evaluates up aim" >:: (fun _ ->
    assert_equal {horizontal = 5; depth = 10; aim = -3} 
      (eval_with_aim {horizontal = 5 ; depth = 10 ; aim = 2} (Up 5)) ~printer: string_of_env
  );

  "correctly evaluates series of commands" >:: (fun _ ->
    assert_equal {horizontal = 15; depth = 60; aim = 10 } 
      (run_with_aim {horizontal = 0; depth = 0; aim = 0} data) 
  )
]

let () = 
  run_test_tt_main suite1 ;
  run_test_tt_main suite2