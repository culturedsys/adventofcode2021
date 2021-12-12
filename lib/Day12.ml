open Core

let is_small cave = String.for_all ~f: Char.is_lowercase cave

let find_paths connections =
  let connected_to cave = List.map connections ~f: (fun (a, b) -> String.(
      if cave = a then Some b  else if cave = b then Some a else None
    )) |> List.filter_opt in
  let rec go path from visited =
    if String.equal from "end" then
      ["end" :: path]
    else
      let next_steps = connected_to from |> List.filter ~f: (fun c -> not (Set.mem visited c)) in
      let new_visited = if is_small from then Set.add visited from else visited in
      List.concat_map next_steps ~f: (fun next ->
          go (from :: path) next new_visited 
        )
  in
  go [] "start" (Set.empty (module String))


  let parse lines = List.map lines ~f: (fun line ->
    match String.split ~on: '-' line with
    | from :: to_ :: _ -> (from, to_)
    | _ -> failwith "Parse error"
  )