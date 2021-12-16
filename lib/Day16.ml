open Core

let binary_of_hex_digit = function
| '0' -> [false; false; false; false]
| '1' -> [false; false; false; true]
| '2' -> [false; false; true; false]
| '3' -> [false; false; true; true]
| '4' -> [false; true; false; false]
| '5' -> [false; true; false; true]
| '6' -> [false; true; true; false]
| '7' -> [false; true; true; true]
| '8' -> [true; false; false; false]
| '9' -> [true; false; false; true]
| 'A' -> [true; false; true; false]
| 'B' -> [true; false; true; true]
| 'C' -> [true; true; false; false] 
| 'D' -> [true; true; false; true]
| 'E' -> [true; true; true; false]
| 'F' -> [true; true; true; true]
| _ -> failwith "Bad hex char"

type operation_id = Sum
| Product
| Minimum
| Maximum
| GreaterThan
| LessThan
| EqualTo

let binary_of_hex s = String.to_list s |>
  List.concat_map ~f: binary_of_hex_digit

type packet = Literal of { version: int; value: int }
| Operation of { version: int; operation_id: operation_id; contents: packet list }

let int_of_bits source = 
  let rec go acc s = match s with
  | [] -> acc
  | head :: rest -> go (2 * acc + (if head then 1 else 0)) rest in
  go 0 source

let parse_fixed n source =
  let (bits, rest) = List.split_n source n in
  (int_of_bits bits, rest, n)

let parse_operation_id = function
    | 0 -> Sum
    | 1 -> Product
    | 2 -> Minimum
    | 3 -> Maximum
    | 5 -> GreaterThan
    | 6 -> LessThan
    | 7 -> EqualTo 
    | _ -> failwith "Unknown type code"

let parse_literal source =
  let rec go (acc, read) s = match s with
  | false :: rest -> let (value, rest', read') = (parse_fixed 4 rest) in
    (acc * 16 + value, rest', read + read' + 1)
  | true :: rest -> let (value, rest', read') = (parse_fixed 4 rest) in
    go (acc * 16 + value, read + read' + 1) rest' 
  | _ -> failwith "Literal too short" in
  go (0, 0) source

let rec parse_packet source = 
  let (version, rest, read) = parse_fixed 3 source in
  let (type_id, rest', read') = parse_fixed 3 rest in
  match type_id with
  | 4 -> let (value, rest'', read'') = parse_literal rest' in
    (Literal {version = version; value = value}, rest'', read + read' + read'')
  | _ -> let (contents, rest'', read'') = parse_operation rest' in 
    (Operation { version = version; operation_id = parse_operation_id type_id; contents = contents }, rest'', read + read' + read'')
and parse_operation source =
  match source with
  | false :: contents -> 
    let (length, rest, read) = parse_fixed 15 contents in
    let (contents, rest', read') = parse_by_length length rest in
    (contents, rest', 1 + read + read')
  | true :: contents ->
    let (count, rest, read) = parse_fixed 11 contents in
    let (contents, rest', read') = parse_by_count count rest in
    (contents, rest', 1 + read + read') 
  | _ -> failwith "Operation too short"
and parse_by_length length source =
  let rec go acc l s =
    if l = 0 then
      (acc, s)
    else
      let (packet, rest, read) = parse_packet s in
      go (packet :: acc) (l - read) rest
    in
  let (contents, rest) = go [] length source in
  (List.rev contents, rest, length)
and parse_by_count count source =
  let rec go acc already_read c s =
    if c = 0 then
      (acc, s, already_read)
    else
      let (packet, rest, read) = parse_packet s in
      go (packet :: acc) (already_read + read) (c - 1) rest
    in
  let (contents, rest, read) = go [] 0 count source in
  (List.rev contents, rest, read)

let rec sum_versions = function
| Literal { version = version; value = _} -> version
| Operation { version = version; operation_id = _; contents = contents } ->
  version + List.sum (module Int) ~f: sum_versions contents 

  
let rec evaluate = function
| Literal { value = value; _ } -> value
| Operation { operation_id = operation_id; contents = contents; _ } ->
  let operands = List.map ~f: evaluate contents in
  match operation_id with
  | Sum -> 
    List.sum (module Int) ~f: Fn.id operands
  | Product ->
    List.fold operands ~init: 1 ~f: (fun l r -> l * r)
  | Minimum -> 
    Option.value_exn (List.min_elt operands ~compare: Int.compare) ~message: "Too few operands for min" 
  | Maximum -> 
    Option.value_exn (List.max_elt operands ~compare: Int.compare) ~message: "Too few operands for max"
  | GreaterThan -> (match operands with
    | [l; r] -> if l > r then 1 else 0
    | _ -> failwith "Wrong number of operands for greater than")
  | LessThan -> (match operands with
    | [l; r] -> if l < r then 1 else 0
    | _ -> failwith "Wrong number of operands for less than")
  | EqualTo -> (match operands with
    | [l; r] -> if l = r then 1 else 0
    | _ -> failwith "Wrong number of operands for equal to")

