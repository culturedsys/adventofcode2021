# [Day 8: Seven Segment Search](https://adventofcode.com/2021/day/8)

## Part 1

The first part is trivial, just go through the list checking the length of the
elements:

```ocaml
let known_digits digits = List.filter digits ~f: (fun d -> 
  let l = String.length d in
    l = 2 || l = 3 || l = 4 || l = 7
)
```

## Part 2

Part 2 is... not trivial. I used a 7-element array to represent the signals,
with each element being a set, representing the possible segments in the display
that the signal could map to. I then went through the input and, using the cases
where a number has a unique set of segments, filtered the possible segments in
the assignment:

```ocaml
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
```

This didn't give me a unique assignment of signals to segments, however. To get
the final answer, I made a list of segment combinations which are valid digits,
and filtered out those assignments which produce invalid combinations:

```ocaml
let assignment_candidates input = 
  let candidates = Set.of_list (module Int) (List.range 0 7) in
  let init = Array.init 7 ~f: (Fn.const candidates) in
  let possibilities = List.fold ~f: filter_for_known_segments ~init: init input in
  permutations (Array.to_list possibilities)

let eliminate_duplicates = List.filter ~f: (fun s -> 
    not (List.contains_dup ~compare: Int.compare s))

let eliminate_invalid input = 
  List.filter ~f: (fun a -> 
      List.for_all ~f: (fun i -> 
        is_valid_pattern (active_segments i (Array.of_list a))) input)

let assignments input = 
  let candidates = assignment_candidates input in
  let results = eliminate_duplicates candidates |> eliminate_invalid input in
  List.to_array @@ List.hd_exn results
```