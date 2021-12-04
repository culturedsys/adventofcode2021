open Core

exception Parse_error

type board = { cells: int array ; size: int }

let parse_draws input = match input with
  | line :: rest -> 
      let draws = String.split line ~on: ',' |> List.map ~f: int_of_string in
    (draws, rest)
  | _ -> raise Parse_error

let parse_empty input = match input with
  | (line :: rest) when String.equal (String.strip line) "" -> rest
  | _ -> raise Parse_error

let parse_line line = String.split ~on: ' ' line |> 
  List.filter ~f: (fun s -> String.length s > 0) |>
  List.map ~f: int_of_string

let parse_board size input =
  let (board_lines, rest) = List.split_while input 
    ~f: (fun line -> String.length (String.strip line) > 0) in
  let board = {
    cells = List.map ~f: parse_line board_lines 
      |> List.concat |> Array.of_list; 
    size = size
  } in
  (board, List.drop_while ~f: (fun l -> String.length (String.strip l) < 1) rest)

let parse_boards size input = 
    let rec go boards input = match parse_board size input with
    | (board, []) -> board :: boards
    | (board, rest) -> go (board :: boards) rest
    
  in go [] input

let parse size input = let lines = String.split input ~on: '\n' in
  let (draws, rest) = parse_draws lines in
  let rest = parse_empty rest in
  let boards = parse_boards size rest in
  (draws, boards)

let cell board row col =  board.cells.(col + row * board.size)

let rows board = List.range 0 board.size |>
  List.map ~f: (fun row ->
    List.range 0 board.size |> 
    List.map ~f: (fun col -> cell board row col)
  )

let cols board = List.range 0 board.size |>
  List.map ~f: (fun col ->
    List.range 0 board.size |> 
    List.map ~f: (fun row -> cell board row col)
  )

let winning drawn board = 
  List.concat [rows board; cols board] |>
  List.exists ~f: (List.for_all ~f: (Set.mem drawn))

let calculate_score winner drawn board = let sum = 
    (Array.filter ~f: (fun c -> not (Set.mem drawn c)) board.cells |> 
      Array.sum (module Int) ~f: Fn.id) in
  winner * sum

exception No_winner

let winning_scores draws boards =
  let rec go (scores: int list) (remaining: int list) (boards: board list) (drawn_before) = match remaining with
    | [] -> scores
    | next :: rest -> let drawn = Set.add drawn_before next in
        let (winners, losers) = List.partition_tf ~f: (winning drawn) boards in
          let new_scores = List.map ~f: (calculate_score next drawn) winners in
          go (List.concat [new_scores; scores]) rest losers drawn 
  in
    go [] draws boards (Set.empty (module Int))

let first_winning_score draws boards = winning_scores draws boards |>
  List.rev |> List.hd_exn

let last_winning_score draws board = winning_scores draws board |> List.hd_exn