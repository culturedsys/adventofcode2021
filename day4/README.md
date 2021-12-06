# [Day 4: Giant Squid](https://adventofcode.com/2021/day/4)

## Part 1

I decided to try out an alternative OCaml standard library for this one - [Jane
Street's Core](https://opensource.janestreet.com/core/). I used a set to keep
track of the numbers that have been drawn before, so got to take advantage of
Core's slightly more convenient way of using parameterized types, in which you
can pass a module that implements the required functionality for a type when
creating an instance; in this case, passing `module Int` to `Set.empty` to 
create a set of `int`s that uses the `Int` module to provide the ordering
functions on `int`s.

The implementation is pretty straightforward - iterate through the list of
drawn numbers, checking each time for a winning board, and returning the score
once a winning board is found.

```ocaml
let winning drawn board = 
  List.concat [rows board; cols board] |>
  List.exists ~f: (List.for_all ~f: (Set.mem drawn))

let first_winning_score draws boards =
  let rec go remaining drawn_before = match remaining with
    | [] -> raise No_winner
    | next :: rest -> let drawn = Set.add drawn_before next in
        match List.filter ~f: (winning drawn) boards with
        | winner :: _ -> calculate_score winner next drawn
        | [] -> go rest drawn in
  go draws (Set.empty (module Int))
```

## Part 2

A small modification allows the same code to be used to find the first winning
score (for the first part), and the last winning score (for the second part), by
finding a list of all winning scores. Iterate through the drawn numbers, keeping
track of which boards have won (initially empty) and which boards have not won
yet (initially, all the boards), and check the boards which haven't won
previously to see if the most recent drawn number makes them win.

```ocaml
let winning_scores draws boards =
  let rec go scores remaining boards drawn_before = match remaining with
    | [] -> scores
    | next :: rest -> let drawn = Set.add drawn_before next in
        let (winners, losers) = List.partition_tf ~f: (winning drawn) boards in
          let new_scores = List.map ~f: (calculate_score next drawn) winners in
          go (List.concat [new_scores; scores]) rest losers drawn 
  in
    go [] draws boards (Set.empty (module Int))
```