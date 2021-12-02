type command = Forward of int
  | Down of int
  | Up of int

exception Parse_error

let parse s = match String.split_on_char ' ' s with
  | ["forward"; n] -> Forward (int_of_string n)
  | ["down"; n] -> Down (int_of_string n)
  | ["up"; n] -> Up (int_of_string n)
  | _ -> raise Parse_error

type environment = {
  horizontal: int;
  depth: int;
}

let eval e c = match c with
| Forward n -> {e with horizontal = e.horizontal + n }
| Down n -> {e with depth = e.depth + n}
| Up n -> {e with depth = e.depth - n}

let run e cs = List.fold_left eval e cs

type environment_with_aim = {
  horizontal: int;
  depth: int;
  aim: int;
}

let eval_with_aim e c = match c with 
  | Forward n -> { e with horizontal = e.horizontal + n ; depth = e.depth + e.aim * n }
  | Down n -> { e with aim = e.aim + n }
  | Up n -> { e with aim = e.aim - n }

let run_with_aim e cs = List.fold_left eval_with_aim e cs

let string_of_env e = String.concat "" [
  "{ horizontal = "; string_of_int e.horizontal ; 
  " ; depth = " ; string_of_int e.depth ; 
  " ; aim = " ; string_of_int e.aim ; " }"
]