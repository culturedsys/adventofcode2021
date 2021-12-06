open Core

let generation fish = 
  let num_spawning = Option.value (Map.find fish 0) ~default: 0 in
  let spawned = (Int.Map.of_alist_exn [6, num_spawning; 8, num_spawning]) in
  let unspawned = Map.fold ~f: (fun ~key ~data acc -> match key with
      | 0 -> acc
      | n -> Map.set ~key: (n - 1) ~data: data acc 
    ) ~init: Int.Map.empty fish in
  Map.merge ~f: (fun ~key:_ data -> match data with
    | `Both (l, r) -> Some(l + r)
    | `Left l -> Some(l)
    | `Right r -> Some(r)
  ) unspawned spawned

let histogram = 
  let add x = match x with 
    | Some(n) -> n + 1
    | None -> 1 in
  List.fold ~f: (Map.update ~f: add) ~init: Int.Map.empty

let total m = List.sum (module Int) ~f: Fn.id (Map.data m)