open Core

let is_open = function
| '(' | '[' | '{' | '<' -> true
| _ -> false  

let close_of_open = function
| '(' -> ')'
| '[' -> ']'
| '{' -> '}'
| '<' -> '>'
| _ -> failwith "Not an open character"

let parse s = String.fold_result ~init: [] ~f: (fun stack c -> 
  if is_open c then
    Ok(c :: stack)
  else match stack with
    | [] -> Error(c)
    | h :: rest -> 
        if Char.equal c  (close_of_open h) then Ok(rest) else Error(c)
) s

let illegal_score = function 
| ')' -> 3
| ']' -> 57
| '}' -> 1197
| '>' -> 25137
| _ -> failwith "Not a close character"

let incomplete_score cs = List.map cs ~f: (fun c -> match close_of_open c with 
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> failwith "Not a close character") |>
  List.fold ~init: 0 ~f: (fun running n ->
    running * 5 + n  
  )