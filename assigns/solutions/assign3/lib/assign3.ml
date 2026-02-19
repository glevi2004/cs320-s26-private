
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let is_upper c =
  let c = int_of_char c in
  65 <= c && c <= 90

let all_upper x =
  let rec loop i =
    if i >= String.length x
    then true
    else if not (is_upper x.[i])
    then false
    else loop (i + 1)
  in loop 0

let lex s =
  let rec go acc i =
    let rec go_digits acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_digit (String.get s (i + j))
      then go_digits acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
    in
    let rec go_uppers acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_upper (String.get s (i + j))
      then go_uppers acc i (j + 1)
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
      | '=' -> go ("=" :: acc) (i + 1)
      | c ->
        if is_digit c
        then go_digits acc i 1
        else if is_ws c
        then go acc (i + 1)
        else if is_upper c
        then go_uppers acc i 1
        else assert false
  in go [] 0

let split_on_char (s : string) (c : char) : string * string =
  let open String in
  let rec loop i =
    if i >= length s
    then (s, "")
    else if s.[i] = c
    then String.(sub s 0 i, sub s (i + 1) (length s - i - 1))
    else loop (i + 1)
  in loop 0

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

let rec eval env e =
  let rec go acc es =
    match es with
    | ("+", e) :: es -> go (acc + eval_md env e) es
    | ("-", e) :: es -> go (acc - eval_md env e) es
    | [] -> acc
    | _ -> assert false
  in go 0 (split_on_pm e)
and eval_md env e =
  let rec go acc es =
    match es with
    | ("*", e) :: es -> go (acc * eval_paren env e) es
    | ("/", e) :: es -> go (acc / eval_paren env e) es
    | [] -> acc
    | _ -> assert false
  in go 1 (split_on_md e)
and eval_paren env e =
  match e with
  | [e] ->
    if all_upper e
    then List.assoc e env
    else int_of_string e
  | "(" :: e -> eval env e
  | _ -> assert false

let rev_append l r =
  let rec go acc l =
    match l with
    | [] -> acc
    | x :: xs -> go (x :: acc) xs
  in go r l

let insert_uniq (k : 'k) (v : 'v) (r : ('k * 'v) list) : ('k * 'v) list =
  let rec go acc l =
    match l with
    | [] -> (k, v) :: r
    | (x, y) :: l ->
      if k = x
      then rev_append acc ((k, v) :: l)
      else go ((x, y) :: acc) l
  in go [] r

let interp (input : string) (env : (string * int) list) : int * (string * int) list =
  match lex input with
  | var :: "=" :: expr -> (
    match eval env expr with
    | ouput -> ouput, insert_uniq var ouput env
    | exception _ -> failwith "whoops!"
  )
  | _ | exception _ -> failwith "whoops!"
