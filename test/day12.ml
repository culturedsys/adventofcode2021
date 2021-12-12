open Core
open OUnit2
open Adventofcode2021.Day12

let connections = [("start", "A"); ("start", "b"); ("A", "c"); ("A", "b"); ("b", "d"); ("A", "end"); ("b", "end")]

let suite1 = "Part 1" >::: [
  "correct answer for example" >:: (fun _ ->
    let result = find_paths `forbid_repeat connections |> List.length in
    assert_equal 10 result
  )
]

let suite2 = "Part 2" >::: [
  "correct answer for example" >:: (fun _ ->
    let result = find_paths `allow_repeat connections |> List.length in
    assert_equal 36 result ~printer: string_of_int   
  );
  "correct answer for larger example" >:: (fun _ ->
    let connections = ["dc-end";
      "HN-start";
      "start-kj";
      "dc-start";
      "dc-HN";
      "LN-dc";
      "HN-end";
      "kj-sa";
      "kj-HN";
      "kj-dc"] |> parse in
    let result = find_paths `allow_repeat connections in
    assert_equal 103 (List.length result) ~printer: string_of_int
  )
]

let () = run_test_tt_main suite1 ; 
    run_test_tt_main suite2