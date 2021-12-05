open Core
open OUnit2
open Adventofcode2021.Day5

let example = {|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|}

let suite1 = "Day 5 part 1" >::: [
  "can parse line" >:: (fun _ ->
    assert_equal ((0, 9), (5, 9)) (parse_line "0,9 -> 5,9")  
  );

  "plot plots all points" >:: (fun _ ->
    let result = plot Point.Map.empty (9,7) (7,7) in
    OUnit2.assert_bool "Not equal" (Map.equal Int.equal 
      (Point.Map.of_alist_exn [(9,7), 1; (8,7), 1; (7,7), 1])
      result
    )
  );

  "plot finds intersections" >:: (fun _ ->
    let result = plot (plot Point.Map.empty (5, 0) (5, 10))  (0, 5) (10, 5) in
    assert_equal (Map.find_exn result   (5, 5)) 2
  );

  "correct result for example" >:: (fun _ ->
    let input = String.split_lines example in
    let result = List.map ~f: parse_line input |> 
      remove_diagonals |> 
      plot_all Point.Map.empty |>
      Map.count ~f: (fun c -> c > 1)  in
    assert_equal 5 result 
  );
]

let () = run_test_tt_main suite1