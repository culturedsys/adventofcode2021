open Core

let is_small cave = String.for_all ~f: Char.is_lowercase cave

let connected_to connections cave = List.map connections ~f: (fun (a, b) -> String.(
  if cave = a then Some b  else if cave = b then Some a else None
)) |> List.filter_opt

type repeats = [`allow_repeat | `forbid_repeat]

let find_paths repeats connections =
  let allow_repeats = match repeats with `allow_repeat -> true | `forbid_repeat -> false in
  let rec go path from visited repeated = 
    if String.equal from "end" then
      ["end" :: path]
    else 
      let new_visited = if is_small from then Set.add visited from else visited in
      let allowable_repeat c = (not (String.equal c "start")) && 
        ((not (Set.mem visited c)) || (allow_repeats && not repeated)) in
      let next_steps = connected_to connections from |>
        List.filter ~f: (fun c -> not (is_small c) || allowable_repeat c) in
        List.concat_map next_steps ~f: (fun next ->
          let new_repeated = repeated || Set.mem visited next in
          go (from :: path) next new_visited new_repeated
        )
  in
  go [] "start" (Set.empty (module String)) false
   
let parse lines = List.map lines ~f: (fun line ->
  match String.split ~on: '-' line with
  | from :: to_ :: _ -> (from, to_)
  | _ -> failwith "Parse error"
)