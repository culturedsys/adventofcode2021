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

type playing = [`player1 | `player2]

let string_of_playing = function
| `player1 -> "player1"
| `player2 -> "player2"

module Game = struct
  module State = struct
    type t = {player: playing; p1score: int; p2score: int; p1pos: int; p2pos: int }
    let sexp_of_t _ =
      failwith "Undefined"
    let t_of_sexp _ = failwith "Undefined"

    let compare a b =
      match (a.player, b.player) with
      | (`player1, `player2) -> -1
      | (`player2, `player1) -> 1
      | _ -> if a.p1score <> b.p1score then
        compare a.p1score b.p1score 
      else if a.p2score <> b.p2score then
        compare a.p2score b.p2score 
      else if a.p1pos <> b.p1pos then
        compare a.p1pos b.p1pos 
      else if a.p2pos <> b.p2pos then
        compare a.p2pos b.p2pos
      else 
        0
    
    let to_string s = 
      sprintf "{ player = %s; p1score = %d; p2score = %d; p1pos = %d; p2pos = %d }"
        (string_of_playing s.player) s.p1score s.p2score s.p1pos s.p2pos
  end
  include Comparable.Make (State)
end

let rec all_outcomes rolls iterations =
  if iterations = 1 then 
    rolls
  else
    let o = all_outcomes rolls (iterations - 1) in
    List.concat_map rolls ~f: (fun roll -> List.map o ~f: (fun prev -> roll + prev))

let wins_cache = ref Game.Map.empty

let rec wins_starting_from playing p1score p2score p1pos p2pos =
  let state: Game.State.t = {player = playing; p1score = p1score; p2score = p2score; p1pos = p1pos; p2pos = p2pos } in
  match Map.find !wins_cache state with
  | Some n -> n
  | None -> 
      let result = wins_starting_from_impl playing p1score p2score p1pos p2pos in
      wins_cache := Map.add_exn !wins_cache ~key: state ~data: result;
      result
and wins_starting_from_impl playing p1score p2score p1pos p2pos = 
  let rolls = all_outcomes [1; 2; 3] 3 in
  if p1score >= 21 then
    (1, 0)
  else if p2score >= 21 then
    (0, 1)
  else 
    let play_round roll =     
      match playing with
      | `player1 ->
        let newpos = add_wrapped p1pos roll in 
          (`player2, p1score + newpos, p2score, newpos, p2pos)
      | `player2 ->
        let newpos = add_wrapped p2pos roll in 
          (`player1, p1score, p2score + newpos, p1pos, newpos)
    in List.map rolls ~f: (fun roll -> 
      let (playing', p1score', p2score', p1pos', p2pos') = play_round roll in
        wins_starting_from playing' p1score' p2score' p1pos' p2pos'
      )  |> 
    List.fold ~init: (0, 0) ~f: (fun (p1acc, p2acc) (p1wins, p2wins) ->
      (p1acc + p1wins, p2acc + p2wins)
    )

