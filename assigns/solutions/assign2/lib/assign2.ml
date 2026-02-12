
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
      | '+' -> go ("+" :: acc) (i + 1)
      | '-' -> go ("-" :: acc) (i + 1)
      | '*' -> go ("*" :: acc) (i + 1)
      | '/' -> go ("/" :: acc) (i + 1)
      | '(' -> go ("(" :: acc) (i + 1)
      | ')' -> go (")" :: acc) (i + 1)
      | c ->
        if is_digit c
        then go_digits acc i 1
        else if is_ws c
        then go acc (i + 1)
        else assert false
  in go [] 0

let split_on_pm e =
  let rec go d op acc_e acc_l e =
    match e, d with
    | [], _
    | ")" :: _, 0 -> List.rev ((op, List.rev acc_e) :: acc_l)
    | "+" :: e, 0 -> go d "+" [] ((op, List.rev acc_e) :: acc_l) e
    | "-" :: e, 0 -> go d "-" [] ((op, List.rev acc_e) :: acc_l) e
    | "(" :: e, d -> go (d + 1) op ("(" :: acc_e) acc_l e
    | ")" :: e, d -> go (d - 1) op (")" :: acc_e) acc_l e
    | x :: e, d -> go d op (x :: acc_e) acc_l e
  in go 0 "+" [] [] e

let split_on_md e =
  let rec go d op acc_e acc_l e =
    match e, d with
    | [], _
    | ")" :: _, 0 -> List.rev ((op, List.rev acc_e) :: acc_l)
    | "*" :: e, 0 -> go d "*" [] ((op, List.rev acc_e) :: acc_l) e
    | "/" :: e, 0 -> go d "/" [] ((op, List.rev acc_e) :: acc_l) e
    | "(" :: e, d -> go (d + 1) op ("(" :: acc_e) acc_l e
    | ")" :: e, d -> go (d - 1) op (")" :: acc_e) acc_l e
    | x :: e, d -> go d op (x :: acc_e) acc_l e
  in go 0 "*" [] [] e

let rec eval e =
  let rec go acc es =
    match es with
    | ("+", e) :: es -> go (acc + eval_md e) es
    | ("-", e) :: es -> go (acc - eval_md e) es
    | [] -> acc
    | _ -> assert false
  in go 0 (split_on_pm e)
and eval_md e =
  let rec go acc es =
    match es with
    | ("*", e) :: es -> go (acc * eval_paren e) es
    | ("/", e) :: es -> go (acc / eval_paren e) es
    | [] -> acc
    | _ -> assert false
  in go 1 (split_on_md e)
and eval_paren e =
  match e with
  | [e] -> int_of_string e
  | "(" :: e -> eval e
  | _ -> assert false

let interp (input : string) : int =
  match eval (lex input) with
  | output -> output
  | exception _ -> failwith "whoops!"
