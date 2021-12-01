# [Day 1: Sonar Sweep](https://adventofcode.com/2021/day/1)

## Part 1

```ocaml
let count_increases l = let first = List.hd l and rest = List.tl l in
  fst @@ List.fold_left (fun (increases, prev) next ->
      if next > prev then (increases + 1, next) else (increases, next))
    (0, first) rest
```

This problem involves going through each element of a list in order, and keeping
track of some running value as you do so - it's a fold! The minor complication
is that the calculation that needs to be done at each step involves the previous
element in the list as well as the current one, but that's easy to handle, just
keep the previous element as part of the state you track in the fold.

It's also possible to implement directly as a recursive function, rather than a
fold. In some ways, directly writing out the recursion is easier to understand,
although the fold is more restricted (you know what kind of recursion it's
doing), which ought to make it easier to understand.

```ocaml
let count_increases l =
  let rec go increases prev l = match l with
    | [] -> increases
    | next :: rest when next > prev -> go (increases + 1) next rest
    | next :: rest -> go increases next rest
  in
    go 0 (List.hd l) (List.tl l)
```

## Part 2

```ocaml
let sliding_sum l =
    let rec go acc l = match l with
      | (a :: b :: c :: xs) -> go ((a + b + c ) :: acc) (b :: c :: xs)
      | _ -> acc
    in
      List.rev (go [] l)
```

The overall calculation is the same (i.e., the number of increasing steps in a
list), but the list to operate on is different - instead of the list given in
the input, it's the list of sums of each three-element subsequence. As we only
need a staticly sized window, and don't need to write a windowing function for a
variable size, we can use a neat feature of pattern matching on lists:matching a
fixed number of elements of the list. So `a :: b :: c :: xs` matches the first
three elements of the list into `a`, `b`, and `c`, leaving the rest in `xs`,
making it easy to find the sum of the three element.
