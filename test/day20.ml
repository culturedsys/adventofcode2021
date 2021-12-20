open OUnit2
open Core
open Adventofcode2021.Point.Map
open Adventofcode2021.Day20

let input = of_alist_exn (List.map ~f: (fun x -> (x, true)) [
  (0, 0); (3, 0); 
  (0, 1); 
  (0, 2); (1, 2); (4, 2);
  (2, 3);
  (2, 4); (3, 4); (4, 4) 
])

let rule = parse_rule "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#" 

let suite1 = "Part 1" >::: [
  "can interpret square as number" >:: (fun _ ->
    let input = of_alist_exn [((-1, 0), true); ((0, 1), true)] in
    let result = value_at_point false input (0, 0) in
    assert_equal 34 result
  );
  "can enhance" >:: (fun _ ->
    let result = enhance 0 4 0 4 rule false input in
    assert_equal 24 (Map.count ~f: Fn.id result) ~printer: string_of_int
  );
  "can enhance twice" >:: (fun _ ->
    let result = enhance_n_times 2 rule input in
    assert_equal 35 (Map.count ~f: Fn.id result)
  )
]

let () = run_test_tt_main suite1