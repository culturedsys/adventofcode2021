# [Day 3: Binary Diagnostic](https://adventofcode.com/2021/day/3)

## Part 1

I thought there might be some clever way to use bitwise operators to avoid
having to actually count the number of 1s in each position, but I couldn't come
up with one. So I just did the obvious thing (representing binary numbers as
lists of `bool`s for convenient manipulation):

```ocaml
let count_bits s =
  let zeros = List.map (Base.Fn.const 0) (List.hd s) in
  let add_bits_to_count count bits =
    List.map2 (fun c b -> c + (if b then 1 else 0)) count bits in
  List.fold_left add_bits_to_count zeros s
```

Then construct the binary number with a 1 in each position that has more 1s than
0s in the source:

```ocaml
let most_common_bits s = let l = List.length s in
  count_bits s |>
  List.map (fun count -> count >= (l - count))
```

This gets the first part of the solution, the "gamma rate". The second part, the
"epsilon rate", is defined as the binary digit with the _least_ common bit in
each position. You can work this out directly from the gamma rate - as each bit
is either the least common or the most common, the least common digit is the
inverse of the most common digit, so the epsilon rate can be calculated by
inverting each bit in the gamma rate.

## Part 2

The problem description is given recursively, so it makes sense to define the
solution recusively:

```ocaml
let filter f haystack =
  let rec go remaining index = match remaining with
    | [answer] -> answer
    | _ ->
      let needle = f remaining in
      let match_index l = (List.nth l index) == (List.nth needle index )
      in go (List.filter match_index remaining) (index + 1 )
  in go haystack 0
```

Again, we're looking for two values, one defined in terms of the most common
bits, and the other defined in terms of the least common. I don't think there's
any way to directly calculate one result from the other (because, after the
first round of filtering, the two operations are operting on completely
distinct inputs). However, the logic is the same in both cases, so we can use
the same algorithm, and parameterize the function that finds the needle value to
search for:

```ocaml
let oxygen_rating = filter most_common_bits input |> int_of_bits

let co2_rating = filter (Base.Fn.compose invert most_common_bits) input
  |> int_of_bits
```

I initially got the wrong answer here, due to an assumption I made when solving
part 1. Did you catch my mistake above?

> each bit is either the least common or the most common

Of course there's one other possibility: there could be the same number of 0s
and 1s. The first part doesn't mention that possibility, so it's probably
reasonable to assume that the input is set up so that it doesn't happen. But the
second part _does_ mention what to do in this case - if 0 and 1 are equally
common, the result should be 1 if we're looking for most common bits, or 0 if
we're looking for least common. My initial implementation of `most_common_bits`
used `count > length / 2`, which is `false` if there are the same number. Simply
changing to `count >= length / 2` won't work, because integer division rounds
down (so if e.g. this would be true if 2 out of 5 bits are 1).
