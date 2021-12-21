open Core
open OUnit2
open Adventofcode2021.Day21

let score_to_string (i, (p1, p2)) =
  String.concat [string_of_int i; " ("; string_of_int p1; ", "; string_of_int p2; ")"]

let suite1 = "Part 1" >::: [
  "correct rolls" >:: (fun _ ->
    let result = Sequence.take deterministic_rolls 10 |> Sequence.to_list in
    assert_equal [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] result ~printer: (List.to_string ~f: Int.to_string)
  );

  "correct result for example" >:: (fun _ ->
    let scores = scores (grouped_rolls 3 deterministic_rolls) 4 8 in
    let (turn, (p1, p2)) = step_to scores 1000 in
    assert_equal 739785 ((if p1 < 1000 then p1 else p2) * (turn + 1) * 3) ~printer: string_of_int   
  )
]

let () = run_test_tt_main suite1