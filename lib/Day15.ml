open Core

type input_node = {
  x: int;
  y: int;
  cost: int;
}

type node = {
  x: int;
  y: int;
  cost: int;
  total: int option;
}

module Point = struct
  type t = int * int
  include Tuple.Comparable (Int) (Int)
end

type graph = node Point.Map.t

let build_graph (nodes: input_node list): graph = 
  List.map nodes ~f: (fun in_node -> 
    ((in_node.x, in_node.y), {x = in_node.x; y = in_node.y; cost = in_node.cost; total = None})  
  ) |>
  Point.Map.of_alist_exn

module Node = struct
  type t = node
  let compare l r = 
    match (l.total, r.total) with 
    | (Some lt, Some rt) -> compare lt rt
    | (Some _, None) -> -1
    | (None, Some _) -> 1
    | (None, None) -> compare l.cost r.cost
end

module Queue = Psq.Make (Point) (Node) 

let neighbours node = List.map [(-1, 0); (0, -1); (0, 1); (1, 0)] ~f: (fun (dx, dy) -> 
  (node.x + dx, node.y + dy))

let find_min_cost graph first last =
  let rec go frontier current =
    let update_neigbour acc neighbour =
      match Queue.find neighbour acc with 
      | None -> acc 
      | Some(n) ->
        let current_total = Option.value_exn current.total ~message: "Current node without total" in
        let total_via_current = match n.total with
        | None -> current_total + n.cost
        | Some previous_total -> 
            if previous_total > current_total + n.cost then 
              current_total + n.cost 
            else previous_total in
        Queue.add neighbour { n with total = Some(total_via_current) } acc in
    if current.x = last.x && current.y = last.y then
      Option.value_exn current.total ~message: "Last node without total"
    else
      let updated_frontier = List.fold (neighbours current) ~init: frontier ~f: update_neigbour in
      let ((_, next), updated) = Option.value_exn (Queue.pop updated_frontier) ~message: "Out of nodes" in
      go updated next in
  go (Queue.of_list (Map.to_alist graph)) {first with total = Some 0}

let parse lines =
  let nodes = List.concat_mapi lines ~f: (fun y line ->
    List.mapi (String.to_list line) ~f: (fun x c -> { x = x; y = y; cost = (int_of_char c) - (int_of_char '0')})  
  ) in
  let width = String.length (List.hd_exn lines) in
  let height = List.length lines in
  (width, height, nodes)