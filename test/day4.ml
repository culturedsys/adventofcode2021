open OUnit2
open Adventofcode2021.Day4
open Core

let example = {|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7|}

let suite1 = "Parse" >::: [
  ("can parse example" >:: fun _ ->
    let (draws, boards) = parse 5 example in (
      assert_equal 27 (List.length draws) ~printer: string_of_int;
      assert_equal 3 (List.length boards) ~printer: string_of_int
    )
  );

  "can get rows" >:: (fun _ ->
    let board = { cells = [|1; 2; 3; 4|]; size = 2} in
    assert_equal [[1; 2]; [3; 4]] (rows board)
  );
  
  "can get cols" >:: (fun _ ->
    let board = { cells = [|1; 2; 3; 4|]; size = 2} in
    assert_equal [[1; 3]; [2; 4]] (cols board)
  );

  "not winning if no row or column" >:: (fun _ ->
    let board = { cells = [|1; 2; 3; 4|]; size = 2} in
    assert_equal false (winning (Set.of_list(module Int) [2; 3]) board)
  );

  "winning if row" >:: (fun _ ->
    let board = { cells = [|1; 2; 3; 4|]; size = 2} in
    assert_equal true (winning (Set.of_list(module Int) [1; 2]) board)
  );

  "winning if col" >:: (fun _ ->
    let board = { cells = [|1; 2; 3; 4|]; size = 2} in
    assert_equal true (winning (Set.of_list(module Int) [3; 4]) board)
  );

  "correct answer for example" >:: (fun _ ->
    let (draws, boards) = parse 5 example in
    assert_equal 4512 (first_winning_score draws boards)
  );

  "correct answer for part 2 example" >:: (fun _ ->
    let (draws, boards) = parse 5 example in
    assert_equal 1924 (last_winning_score draws boards)
  );
]

let () = run_test_tt_main suite1