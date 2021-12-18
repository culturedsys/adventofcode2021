open Core
open OUnit2
open Adventofcode2021.Day18

let left_branch = function
| Tree.Branch { left = left ; _ } -> !left
| _ -> failwith "Expected branch"

let right_branch = function
| Tree.Branch { right = right ; _ } -> !right
| _ -> failwith "Expected branch"

let next_left_leaf = function
| Tree.Leaf { nextLeft = nextLeft; _ } -> !nextLeft
| _ -> failwith "Expected leaf"

let next_right_leaf = function
| Tree.Leaf { nextRight = nextRight; _} -> !nextRight
| _ -> failwith "Expected leaf"

let leaf = function
| Tree.Leaf l -> l
| _ -> failwith "Expected leaf"

let suite1 = "Part 1" >::: [
  "can parse simple" >:: (fun _ ->
    let expected = Tree.Branch { 
      left = ref (Tree.Leaf {
        value = ref 1; 
        nextLeft = ref None;
        nextRight = ref None 
      });
      right = ref (Tree.Leaf {
        value = ref 2;
        nextLeft = ref None;
        nextRight = ref None 
      })
    } in
    let result = parse "[1,2]" in
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string
      expected result 
  );
  "can parse nested" >:: (fun _ ->
    let result = parse "[[1,2],3]" in
    let left_most = left_branch (left_branch result) in
    assert_bool "Next left of left most should be none" (Option.is_none (next_left_leaf left_most));
    
    let right_of_left = right_branch (left_branch result) in
    let right_most = right_branch result in
    assert_bool "Next right of right of left should be next" 
      @@ Option.equal (Tree.equal) (Some (right_most)) (Option.map ~f: (fun l -> Tree.Leaf l) (next_right_leaf right_of_left));

    assert_bool "Next left of right most should be right of left"
      @@ Option.equal (Tree.equal) (Some right_of_left) (Option.map ~f: (fun l -> Tree.Leaf l) (next_left_leaf right_most))
  );

  "can explode" >:: (fun _ ->
    let tree = ref (parse "[[[[[9,8],1],2],3],4]") in
    let expected = parse "[[[[0,9],2],3],4]" in
    assert_bool "Should explode" @@ explode_first tree;
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string expected !tree
  );

  "can explode internal" >:: (fun _ ->
    let tree = ref (parse "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") in
    let expected = parse "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" in
    assert_bool "Should explode" @@ explode_first tree;
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string expected !tree  
  );

  "can explode more complicated" >:: (fun _ ->
    let tree = ref (parse "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,9],[[[5,6],9],[B,0]]]]") in
    let expected = parse "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,E],[[0,F],[B,0]]]]" in
    assert_bool "Should explode" @@ explode_first tree;
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string expected !tree
  );

  "can explode from first of second after join" >:: (fun _ ->
    let left = parse "[1,2]" in
    let right = parse "[[[[3,4],5],6],7]" in
    let expected = parse "[[1,5],[[[0,9],6],7]]" in
    let joined = ref (join left right) in
    ignore @@ explode_first joined;
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string
      expected !joined
  );

  "doesn't explode if nothing to explode" >:: (fun _ ->
    let tree = ref (parse "[[[[0,7],4],[1,[0,1]]],[1,1]]") in
    assert_bool "Should not explode" @@ not (explode_first tree) 
  );

  "can split then explode" >:: (fun _ ->
    let tree = ref (parse "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,9],[[B,9],[B,0]]]]") in
    let expected = parse "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,E],[[0,F],[B,0]]]]" in
    ignore @@ split_first tree;
    ignore @@ explode_first tree;
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string
      expected !tree
  );

  "can reduce" >:: (fun _ ->
    let tree = ref (parse "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]") in
    let expected = parse "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]" in
    reduce tree;
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string 
      expected !tree
  );

  "can add" >:: (fun _ ->
    let left = parse "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" in  
    let right = parse "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]" in
    let expected = parse "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]" in
    let result = add left right in
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string 
      expected result
  );

  "can sum" >:: (fun _ ->
    let trees = [
      "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]";
      "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]";
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]";
      "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]";
      "[7,[5,[[3,8],[1,4]]]]";
      "[[2,[2,2]],[8,[8,1]]]";
      "[2,9]";
      "[1,[[[9,3],9],[[9,0],[0,7]]]]";
      "[[[5,[7,4]],7],1]";
      "[[[[4,2],2],6],[8,7]]"
    ] in
    let expected = parse "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" in
    let result = sum trees in
    assert_equal ~cmp: Tree.equal ~printer: Tree.to_string
      expected result
  )
]

let suite2 = "Part 2 " >::: [
  "can find max" >:: (fun _ ->
    let input = [
      "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]";
      "[[[5,[2,8]],4],[5,[[9,9],0]]]";
      "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]";
      "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]";
      "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]";
      "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]";
      "[[[[5,4],[7,7]],8],[[8,3],8]]";
      "[[9,3],[[9,9],[6,[4,9]]]]";
      "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]";
      "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
    ] in
    assert_equal ~printer: string_of_int 3993 (max input)
  )
]

let () = run_test_tt_main suite1; run_test_tt_main suite2