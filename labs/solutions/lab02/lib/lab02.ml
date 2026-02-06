
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let lex s =
  let rec go acc i =
    let rec go_digits acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_digit (String.get s (i + j))
      then go_digits acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
    in
    if i >= String.length s
    then List.rev acc
    else
      match String.get s i with
      | '-' -> go ("-" :: acc) (i + 1)
      | '(' -> go ("(" :: acc) (i + 1)
      | ')' -> go (")" :: acc) (i + 1)
      | c ->
        if is_digit c
        then go_digits acc i 1
        else if is_ws c
        then go acc (i + 1)
        else assert false
  in go [] 0

let rec drop_last l =
  match l with
  | x :: y :: rest -> x :: drop_last (y :: rest)
  | _ -> []

let split_on_minus e =
  let rec go d acc e =
    match d, e with
    | _, [] -> (List.rev acc, [])
    | d, "(" :: xs -> go (d + 1) ("(" :: acc) xs
    | d, ")" :: xs -> go (d - 1) (")" :: acc) xs
    | 0, "-" :: xs -> (List.rev acc, xs)
    | d, x :: xs -> go d (x :: acc) xs
  in go 0 [] e

let rec eval e =
  let rec go acc e =
    match e with
    | [] -> acc
    | e ->
      let (le, e) = split_on_minus e in
      go (acc - eval_num_paren le) e
  in
  let (le, e) = split_on_minus e in
  go (eval_num_paren le) e
and eval_num_paren e =
  match e with
  | [n] -> int_of_string n
  | "(" :: rest -> eval (drop_last rest)
  | _ -> assert false

let interp e =
  match eval (lex e) with
  | n -> n
  | exception _ -> failwith "whoops!"
