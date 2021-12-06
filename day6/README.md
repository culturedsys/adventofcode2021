# [Day 6: Lanternfish](https://adventofcode.com/2021/day/6)

## Part 1

80 days contains roughly 10 generations, so the maximum number of fish starting
from multiple 100s, is in the order of 100 \* 2<sup>10</sup>, i.e. a couple of
hundred thousand. I figured this was small enough for the naive implementation,
suggested by the prompt, which keeps a list of the countdowns for all fish:

```ocaml
let generation = List.concat_map ~f: (function
  | 0 -> [6; 8]
  | n -> [n - 1])
```

## Part 2

256 days contains roughly 32 generations, so the maximum number of fish is in
the order of 100 * 2<sup>32</sup>, i.e. hundreds of billions. Predictably, the
naive implementation was too slow (I don't know how much too slow - I stopped it
after about 10 seconds). A better implementation is to keep a count of how many
fish have each countdown - this way, the number of operations to calculate the
next generation remains the same no matter how many fish there are. The thing
that tripped me up at first was that there are *two* sources of fish with a 6
day countdown - the number of fish with a 7 day countdown, and the number of
fish with a 0 day countdown. My initial implementation was a pretty direct
translation of the above, i.e., it moved the count of fish with a 0 day
countdown to the same number of fish in both 6 and 8 days, and moved all the
other counts to the countdown of (n - 1). This meant the total for a countdown
of 6 would be either the total coming from 0, or from 7, but not both. To handle
this, I produce two sets of counts, one for the fish that are spawning, and one
for the rest, then merge them, adding the totals where they appear in both maps.

```ocaml
let generation fish =
  let num_spawning = Option.value (Map.find fish 0) ~default: 0 in
  let spawned = (Int.Map.of_alist_exn [6, num_spawning; 8, num_spawning]) in
  let unspawned = Map.fold ~f: (fun ~key ~data acc -> match key with
      | 0 -> acc
      | n -> Map.set ~key: (n - 1) ~data: data acc
    ) ~init: Int.Map.empty fish in
  Map.merge ~f: (fun ~key:_ data -> match data with
    | `Both (l, r) -> Some(l + r)
    | `Left l -> Some(l)
    | `Right r -> Some(r)
  ) unspawned spawned
```
