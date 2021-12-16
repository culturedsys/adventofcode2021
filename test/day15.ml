open Core
open Adventofcode2021.Day15
open OUnit2

let suite1 = "Part 1" >::: [
  "trivial example" >:: (fun _ ->
    let input = build_graph [
      { x = 0; y = 0; cost = 0 };
      { x = 1; y = 0; cost = 1 };
      { x = 0; y = 1; cost = 2 };
      { x = 1; y = 1; cost = 3 };
    ] in
    let first = Map.find_exn input (0, 0) in
    let last = Map.find_exn input (1, 1) in
    let result = find_min_cost input first last in
    assert_equal 4 result
  )
]

let string_of_graph width height graph =
  List.range 0 height |> List.map ~f: (fun row ->
    List.range 0 width |> List.map ~f: (fun col -> string_of_int (Map.find_exn graph (row, col)).cost 
    ) |> String.concat) |> String.concat ~sep: "\n"

let suite2 = "Part 2" >::: [
  "expand graph" >:: (fun _ ->
    let input = build_graph [
      { x = 0; y = 0; cost = 1 };
      { x = 1; y = 0; cost = 6 };
      { x = 0; y = 1; cost = 9 };
      { x = 1; y = 1; cost = 9 };
    ] in
    let result = expand_graph input 2 0 1 in
    assert_equal ~cmp: (Map.equal Node.equal) (build_graph [
      { x = 2; y = 0; cost = 2 };
      { x = 3; y = 0; cost = 7 };
      { x = 2; y = 1; cost = 1 };
      { x = 3; y = 1; cost = 1 };
    ]) result 
  );
  "expand all" >:: (fun _ ->
    let input = build_graph [{x = 0; y = 0; cost = 0}] in
    let result = expand_all input 1 1 in
    let expected = (build_graph [
      {x = 0; y = 0; cost = 0};
      {x = 1; y = 0; cost = 1};
      {x = 2; y = 0; cost = 2};
      {x = 3; y = 0; cost = 3};
      {x = 4; y = 0; cost = 4};

      {x = 0; y = 1; cost = 1};
      {x = 1; y = 1; cost = 2};
      {x = 2; y = 1; cost = 3};
      {x = 3; y = 1; cost = 4};
      {x = 4; y = 1; cost = 5};

      {x = 0; y = 2; cost = 2};
      {x = 1; y = 2; cost = 3};
      {x = 2; y = 2; cost = 4};
      {x = 3; y = 2; cost = 5};
      {x = 4; y = 2; cost = 6};

      {x = 0; y = 3; cost = 3};
      {x = 1; y = 3; cost = 4};
      {x = 2; y = 3; cost = 5};
      {x = 3; y = 3; cost = 6};
      {x = 4; y = 3; cost = 7};

      {x = 0; y = 4; cost = 4};
      {x = 1; y = 4; cost = 5};
      {x = 2; y = 4; cost = 6};
      {x = 3; y = 4; cost = 7};
      {x = 4; y = 4; cost = 8};
    ]) in
    assert_equal (Map.length expected) (Map.length result) ~printer: string_of_int;
    assert_equal ~cmp: (Map.equal Node.equal) expected result ~printer: (string_of_graph 5 5)
  )
]

let () = run_test_tt_main suite1;
  run_test_tt_main suite2