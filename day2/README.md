# [Day 2: Dive](https://adventofcode.com/2021/day/2)

## Part 1

Ooh, is this going to be the start of a series of challenges leading up to
developing a complete Logo interpreter? I considered leaning into that
possibility by going HAM with parser combinators, but I decided that was
overkill for this challenge. I did define a data type for expressions:

```ocaml
type command = Forward of int
  | Down of int
  | Up of int
```

This allows for a separation between parsing and evaluation, making the
evaluation clearer:

```ocaml
type environment = {
  horizontal: int;
  depth: int;
}

let eval e c = match c with
| Forward n -> { e with horizontal = e.horizontal + n }
| Down n -> { e with depth = e.depth + n }
| Up n -> { e with depth = e.depth - n }
```

## Part 2

Same as the first, a little bit longer and a little bit worse.

```ocaml
type environment_with_aim = {
  horizontal: int;
  depth: int;
  aim: int;
}

let eval_with_aim e c = match c with
  | Forward n -> { e with horizontal = e.horizontal + n ; depth = e.depth + e.aim * n }
  | Down n -> { e with aim = e.aim + n }
  | Up n -> { e with aim = e.aim - n }
```
