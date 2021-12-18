open Core

module Tree = struct
  type t = 
  | Branch of branch
  | Leaf of leaf and
  branch = { left: t ref; right: t ref } and
  leaf = { value: int ref; nextLeft: leaf option ref; nextRight: leaf option ref }

  let rec equal x y = match x, y with
  | Branch { left = xl; right = xr }, Branch { left = yl; right = yr } -> 
    (equal !xl !yl) && (equal !xr !yr) 
  | Leaf { value = xv; _ }, Leaf { value = yv; _  } -> !xv = !yv
  | _ -> false

  let rec to_string_explode = function
  | Leaf { value = value; nextLeft = l; nextRight = r } -> String.concat [Int.to_string !value; " <"; 
      Option.value ~default: "X" (Option.map ~f: (fun leaf -> Int.to_string !(leaf.value)) !l); " ";
      Option.value ~default: "X" (Option.map ~f: (fun leaf -> Int.to_string !(leaf.value)) !r);
      ">\n"];
  | Branch { left = left; right = right} -> String.concat ["["; to_string_explode !left; ",\n"; to_string_explode !right; "]"] 

  let rec to_string = function
  | Leaf {value = value; _} -> string_of_int !value
  | Branch { left = left; right = right } -> String.concat ["["; to_string !left; ","; to_string !right; "]"]
end

let consume_char c s = match s with
| h :: rest -> if Char.equal h c then rest else failwith (String.concat ["Expected "; Char.to_string c; " got "; Char.to_string h])
| _ -> failwith "Unexpected end of string"

let rec parse_impl s = match s with
  | '[' :: rest -> parse_pair rest
  | c :: rest -> let leaf = parse_value c in
    (Tree.Leaf leaf, leaf, leaf, rest)
  | _ -> failwith "Unexpected end of string"

and parse_pair s =     
  let (left, leftMostL, rightMostL, rest) = parse_impl s in
  let rest = consume_char ',' rest in
  let (right, leftMostR, rightMostR, rest) = parse_impl rest in
  let rest = consume_char ']' rest in
  (match rightMostL, leftMostR with
  | { nextRight = nextRightL; _ }, { nextLeft = nextLeftR; _ } -> 
      nextRightL := Some leftMostR;
      nextLeftR := Some rightMostL);
  (match leftMostR, rightMostL with
  | { nextLeft = nextLeftR; _ }, { nextRight = nextRightL; _ } -> 
      nextLeftR := Some rightMostL;
      nextRightL := Some leftMostR);
  (Tree.Branch { left = ref left; right = ref right}, leftMostL, rightMostR, rest)

and parse_value c = if String.contains "0123456789ABCDEF" c then 
  {
    value = ref (String.index_exn "0123456789ABCDEF" c);
    nextLeft = ref None;
    nextRight = ref None;
  }
else
  failwith "Expected digit"

let parse s = 
  let (result, _, _, _) = parse_impl (String.to_list s) in
  result

let rec find_branch_at_depth n t = 
  if n = 0 then 
    match !t with
    | Tree.Branch _ -> Some(t)
    | _ -> None
  else match !t with
  | Tree.Leaf _ -> None
  | Tree.Branch { left = left; right = right} ->
    let left_opt = find_branch_at_depth (n - 1) left in
    if Option.is_some left_opt then
      left_opt
    else
      find_branch_at_depth (n - 1) right

let explode n = match !n with
| Tree.Branch { left = { contents = (Tree.Leaf left) }; right = {contents = (Tree.Leaf right)} } ->
  let new_leaf: Tree.leaf = { value = ref 0; nextLeft = left.nextLeft; nextRight = right.nextRight } in
  (match !(left.nextLeft) with
  | Some l -> l.value := !(l.value) + !(left.value); l.nextRight := Some new_leaf
  | None -> ());
  (match !(right.nextRight) with
  | Some r -> r.value := !(r.value) + !(right.value); r.nextLeft := Some new_leaf
  | None -> ());
  n := Tree.Leaf new_leaf
| _ -> failwith @@ String.concat ["Expected a branch containing two leaves, got "; Tree.to_string !n]

let explode_first t = 
  match find_branch_at_depth 4 t with
| Some(n) -> explode n; true
| None -> false

let rec find_leaf_gte n t = match !t with
| Tree.Branch { left = left; right = right } -> (
    match find_leaf_gte n left with
    | Some n -> Some n 
    | None -> find_leaf_gte n right 
  )
| Tree.Leaf leaf ->
  if !(leaf.value) >= n then
    Some(t)
  else
    None

let split n = 
  match !n with
  | Tree.Leaf leaf ->
    let left = Float.round_down ((float_of_int !(leaf.value)) /. 2.0) |> int_of_float in 
    let right = Float.round_up ((float_of_int !(leaf.value)) /. 2.0) |> int_of_float in
    let left_leaf: Tree.leaf = { value = ref(left); nextLeft = leaf.nextLeft; nextRight = ref None } in
    let right_leaf: Tree.leaf  = { value = ref(right); nextLeft = ref None; nextRight = leaf.nextRight } in
    left_leaf.nextRight := Some right_leaf;
    right_leaf.nextLeft := Some left_leaf;
    let new_node = Tree.Branch {
      left = ref (Tree.Leaf left_leaf);
      right = ref (Tree.Leaf right_leaf)
    } in
    (match !(leaf.nextLeft) with
    | None -> ()
    | Some l -> l.nextRight := Some left_leaf 
    );
    (match !(leaf.nextRight) with
    | None -> ()
    | Some l -> l.nextLeft := Some right_leaf 
    );
    n := new_node
  | _ -> failwith @@ String.concat ["Expected a leaf, got"; Tree.to_string !n]

let split_first t = 
match find_leaf_gte 10 t with
| Some l -> split l; true
| None -> false

let rec reduce t =
  while explode_first t do
    ()
  done;
  if split_first t then
    reduce t

let rec right_most = function
| Tree.Leaf leaf -> leaf
| Tree.Branch { right = right; _ } -> right_most !right

let rec left_most = function
| Tree.Leaf leaf -> leaf
| Tree.Branch { left = left; _ } -> left_most !left

let join left right =
  let right_of_left = right_most left in
  let left_of_right = left_most right in
  right_of_left.nextRight := Some left_of_right;
  left_of_right.nextLeft := Some right_of_left;
  Tree.Branch { left = ref left; right = ref right }

let add left right = 
  let sum = ref (join left right) in
  reduce sum;
  !sum

let rec magnitude = function
| Tree.Leaf { value = value; _ } -> !value
| Tree.Branch { left = left; right = right } -> 3 * (magnitude !left) + 2 * (magnitude !right) 

let sum = function
| [] -> failwith "Not enough elements"
| h :: t ->
  let first = parse h in 
  List.fold t ~init: first ~f: (fun acc line ->
    let second = parse line in
    add acc second
  )

let max input =
  let pairs = List.concat_map input ~f: (fun left -> 
      List.map input ~f: (fun right -> (parse left, parse right))
    ) |> List.filter ~f: (fun (l, r) -> not (Tree.equal l r)) in
  let sums = List.map pairs ~f: (fun (l, r) ->
    add l r |> magnitude
  ) in
  Option.value_exn (List.max_elt sums ~compare: Int.compare)