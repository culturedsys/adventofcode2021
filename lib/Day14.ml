open Core

module Pair = struct
  type t = char * char
  include Tuple.Comparable (Char) (Char)
  include Tuple.Hashable (Char) (Char)  
  include Tuple.Sexpable (Char) (Char)
end

module Triple = struct
  type t = (char * char) * int
  include Tuple.Comparable (Pair) (Int)
  include Tuple.Hashable (Pair) (Int)
  include Tuple.Sexpable (Pair) (Int)
end

let step rules seed =
  let subst pair = match Map.find rules pair with 
  | Some(a) -> [a; fst pair]
  | None -> [] in 
  let rec go acc = function
  | [x; y; z] -> List.append (z :: subst (y, z)) (List.append (subst (x, y)) acc)
  | x :: y :: rest -> go (List.append (subst (x, y)) acc) (y :: rest)
  | _ -> failwith "Too few elements" in
  List.rev (go [] seed)

let rule_pattern = Re2.create_exn "([A-Z])([A-Z]) -> ([A-Z])"

let parse_rules rules = 
  List.map rules ~f: (fun rule -> match Re2.find_submatches rule_pattern rule with
    | Ok([|_; Some(i1); Some(i2); Some(o)|]) -> ((String.get i1 0, String.get i2 0), String.get o 0)
    | _ -> failwith "Parse error"
  ) |> Pair.Map.of_alist_exn

let parse = function 
| seed :: _ :: rules -> (String.to_list seed, parse_rules rules)
| _ -> failwith "Parse error"

let frequency (l: char list) =
  List.fold l ~init: (Map.empty (module Char)) ~f: (fun acc c ->
    Map.update acc c ~f: (function | Some(v) -> v + 1 | None -> 1)
  )

let sum_maps m1 m2 = Map.merge_skewed m1 m2 ~combine: (fun ~key:_ v1 v2 -> v1 + v2)

let total_after_steps rules seed n = 
  let rec pairs = function
  | [x; y] -> [(x, y)]
  | x :: y :: rest -> (x, y) :: (pairs (y :: rest))
  | _ -> failwith "not enough elements" in
  
  let memo = Hashtbl.create (module Triple) in
  let remember k d = (match Hashtbl.add memo ~key: k ~data: d with
    | `Duplicate -> if not (Map.equal (Int.equal) (Hashtbl.find_exn memo k) d) then failwith "Already memoized with different value" 
    | `Ok -> ()); 
    d in

  let rec totals_for_pair n (a, b) =
    match Hashtbl.find memo ((a, b), n) with
    | Some result -> result
    | None -> let result = totals_for_pair_impl n (a, b) in
        remember ((a, b), n) result
  and totals_for_pair_impl n (a, b) = 
    if n = 0 then
      Map.singleton (module Char) a 1
    else match Map.find rules (a, b) with
      | None -> Map.singleton (module Char) a 1
      | Some insert -> sum_maps (totals_for_pair (n - 1) (a, insert)) (totals_for_pair (n - 1) (insert, b)) in
  
  List.map (pairs seed) ~f: (totals_for_pair n) |> 
  List.fold ~init: (Map.singleton (module Char) (List.last_exn seed) 1) ~f: sum_maps   













