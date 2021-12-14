open Core

module Pair = struct
  include Tuple.Comparable (Char) (Char)
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