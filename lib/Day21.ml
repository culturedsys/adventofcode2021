open Core

let deterministic_rolls =
  Sequence.unfold ~init: 1 ~f: (fun prev -> Some (prev, prev + 1))

let grouped_rolls n rolls =
  let rec grouped r: ('a Sequence.t) Sequence.t = 
    Sequence.append (Sequence.singleton (Sequence.take r n)) 
      (Sequence.of_lazy (lazy (grouped (Sequence.drop r n)))) in
  Sequence.map ~f: (Sequence.sum (module Int) ~f: Fn.id) (grouped rolls)

let add_wrapped x y = ((x + y - 1) % 10) + 1

let scores rolls p1 p2 =
  let positions = Sequence.folding_mapi rolls ~init: (p1, p2) ~f: (fun i (prev1, prev2) roll ->
    let updated = if i % 2 = 1 then (prev1, add_wrapped prev2 roll) else (add_wrapped prev1 roll, prev2) in
    (updated, (i, updated))
  ) in
  Sequence.folding_map positions ~init: (0, 0) ~f: (fun (prev1, prev2) (i, (p1, p2)) ->
    let updated = if i % 2 = 1 then prev1, prev2 + p2 else prev1 + p1, prev2 in
    updated, (i, updated)  
  )

let step_to scores n = 
  Sequence.drop_while ~f: (fun ((_, (p1, p2))) -> p1 < n && p2 < n) scores |> Sequence.hd_exn