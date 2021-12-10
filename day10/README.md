# [Day 10: Syntax Scoring](https://adventofcode.com/2021/day/10)

## Part 1

Go through the string, and when you encounter an open character push it on a
stack; when you encounter a close character, check if it is the appropriate
character for the top of the stack, and if it isn't, return an error.

OCaml core's `fold_result` is useful here, as you can return a `Result` type to
indicate whether the fold should continue.

```ocaml
let parse s = String.fold_result ~init: [] ~f: (fun stack c -> 
  if is_open c then
    Ok(c :: stack)
  else match stack with
    | [] -> Error(c)
    | h :: rest -> 
        if Char.equal c  (close_of_open h) then Ok(rest) else Error(c)
) s
```

## Part 2

One of the fun things about Advent of Code is that you never know, when doing
the first part, what the second part is going to be. So when writing the first
part, you're always thinking at least a little bit about how your solution might
generalise to potential second parts. I was thinking that this problem is
unusual, in that I couldn't think of a way to solve the first part that doesn't
do most of the work for the second part. In order to search for closing
characters that don't match, you have to keep a stack of opening characters, and
the answer for the second part is (a calculation based on) the stack of opening
characters.

However, you can write a recursive function that keeps the stack of opening
characters on the call stack without materialising it in a way that can be used
to calculate the result.

```ocaml
let parse s =
  let rec go chars opening = match chars with
    | [] -> Ok([])
    | c :: cs ->
      if is_open c then
        match go cs c with
        | Error e -> Error e
        | Ok(rest) -> go rest opening
      else
        if Char.equal c (close_of_open opening) then
          Ok(rest)
        else
          Error(c) in
  let l = String.to_list s in
  go (List.hd_exn l) (List.tail_exn l)
```