open Core

let split_and_trim s = String.split ~on: ' ' s |> List.filter ~f: (fun x -> String.length x > 0)

let parse line = 
  match String.split ~on: '|' line with
  | [input; output] -> (split_and_trim input, split_and_trim output)
  | _ -> failwith "Fail"

let rec permutations a = match a with
  | [] -> [[]]
  | (x :: xs) -> 
      let rest = permutations xs in
      List.concat_map ~f: (fun r -> List.map ~f: (fun c ->  c :: r) (Set.to_list x)) rest

let digit_of_char c = (Char.to_int c) - (Char.to_int 'a')

let digits s = String.to_list s |> List.map ~f: digit_of_char
      
let valid_patterns = List.map ~f: (Set.of_list (module Int)) [
  [0; 1; 2; 4; 5; 6];
  [2; 5];
  [0; 2; 3; 4; 6];
  [0; 2; 3; 5; 6];
  [1; 2; 3; 5];
  [0; 1; 3; 5; 6];
  [0; 1; 3; 4; 5; 6];
  [0; 2; 5];
  [0; 1; 2; 3; 4; 5; 6];
  [0; 1; 2; 3; 5; 6]
]

let known_digits digits = List.filter digits ~f: (fun d -> 
  let l = String.length d in
    l = 2 || l = 3 || l = 4 || l = 7
)

let segments_if_known s = Option.map ~f: (Set.of_list (module Int)) @@ match List.length s with
  | 2 -> Some([2; 5])
  | 3 -> Some([0; 2; 5])
  | 4 -> Some([1; 2; 3; 5])
  | 7 -> Some([0; 1; 2; 3; 4; 5; 6])
  | _ -> None

let filter_for_known_segments running data = match (segments_if_known data) with
  | None -> running
  | Some(known_segments) ->
    Array.mapi ~f: (fun i s ->
      if List.mem ~equal: Int.equal data i then
        Set.inter s known_segments
      else
        Set.diff s known_segments
    ) running

let is_valid_pattern d = List.mem ~equal: (Set.equal) valid_patterns d

let active_segments input assignments = Set.of_list (module Int) @@ List.map input ~f: (fun s -> assignments.(s))

let assignment_candidates input = 
  let candidates = Set.of_list (module Int) (List.range 0 7) in
  let init = Array.init 7 ~f: (Fn.const candidates) in
  let possibilities = List.fold ~f: filter_for_known_segments ~init: init input in
  permutations (Array.to_list possibilities)

let eliminate_duplicates = List.filter ~f: (fun s -> not (List.contains_dup ~compare: Int.compare s))

let eliminate_invalid input = 
  List.filter ~f: (fun a -> List.for_all ~f: (fun i -> is_valid_pattern (active_segments i (Array.of_list a))) input)

let assignments input = 
  let candidates = assignment_candidates input in
  let results = eliminate_duplicates candidates |> eliminate_invalid input in
  List.to_array @@ List.hd_exn results

let pattern_to_int a = Option.value_exn (List.findi ~f: (fun _ d -> Set.equal a d) valid_patterns |> 
    Option.map ~f: fst)