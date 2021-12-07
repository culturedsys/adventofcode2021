# [Day 7: The Treachery of Whales](https://adventofcode.com/2021/day/7)

## part 1

I thought through a few possible cases and satisfied myself that the value with
the lowest difference from all the points in the list would always be the
median, which is simple to implement:

```ocaml
let median xs = 
  let sorted = List.sort xs ~compare: Float.compare in
  let l = List.length sorted in
  let result = if (l / 2) * 2 = l then
    let [@warning "-8"] [a; b] = List.take (List.drop sorted (l / 2 - 1)) 2 in
    (a +. b) /. 2.
  else
    List.hd_exn (List.drop sorted (l / 2)) in
  result

let cost xs =
  let target = median xs in
  List.sum (module Float) ~f: (fun x -> Float.abs (x -. target)) xs
```

## Part 2

I couldn't think of an obvious way to find the target position for this part,
but I did figure out that the cost, being the sum of all the number up to the
distance, could be calculated as `n(n + 1) / 2`, and that the target has to be
within the range of the input data. So I tried a brute force method, calculating
the costs of all positions and choosing the minimum:

```ocaml
let min_cost xs =
  let min = List.min_elt ~compare: Float.compare xs |> Option.value ~default: 0. |> int_of_float in
  let max = List.max_elt ~compare: Float.compare xs |> Option.value ~default: 0. |> int_of_float in
  let cost_one a b = let n = Float.abs (a -. b) in (n *. (n +. 1.)) /. 2. in
  let cost target = List.sum (module Float) ~f: (cost_one target) xs in
  let costs = List.range ~stop: `inclusive min max |> List.map ~f: (fun target -> cost (float_of_int target)) in
  List.min_elt ~compare: Float.compare costs |> Option.value ~default: 0.
```

It was fast enough (about 50ms); it could be sped up by doing a binary search
for the minimum, as long as the costs are single-peaked (which I guess they
probably are, but I didn't want to have to prove it).

It turns out that the value with the minimum square of the distance from a set
of values is the mean ([here's a
proof](https://math.stackexchange.com/questions/967138/formal-proof-that-mean-minimize-squared-error-function)).
I still don't really have a strong intuition as to why this works, though - I
guess, if, in part 1, the magnitude of the distances isn't relevant, just their
order (hence the median being the target), making the cost be the square of the
distance introduces the distance again as an additional term, meaning that the
magnitude does matter (and making the median the target). 