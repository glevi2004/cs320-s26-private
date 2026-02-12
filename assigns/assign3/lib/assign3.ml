
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

(* lex : string -> string list
   Remove whitespace and split a calculator statement into lexemes:
   parentheses, + - * / =, digit groups, and all-uppercase variables.
   Precondition: input matches the assignment’s statement grammar.
 *)
let lex (s : string) : string list =
  let rec take_while p i j =
    if i + j >= String.length s then j
    else if p s.[i + j] then take_while p i (j + 1)
    else j
  in
  let rec go acc i =
    if i >= String.length s then List.rev acc
    else
      match s.[i] with
      | c when is_ws c -> go acc (i + 1)
      | '(' -> go ("(" :: acc) (i + 1)
      | ')' -> go (")" :: acc) (i + 1)
      | '+' -> go ("+" :: acc) (i + 1)
      | '-' -> go ("-" :: acc) (i + 1)
      | '*' -> go ("*" :: acc) (i + 1)
      | '/' -> go ("/" :: acc) (i + 1)
      | '=' -> go ("=" :: acc) (i + 1)
      | c when is_digit c ->
        let len = take_while is_digit i 0 in
        go (String.sub s i len :: acc) (i + len)
      | c when is_upper c ->
        let len = take_while is_upper i 0 in
        go (String.sub s i len :: acc) (i + len)
      | _ -> assert false
  in
  go [] 0

(* eval : (string * int) list -> string list -> int
   Evaluate an infix arithmetic expression with variabls using the
   current environment (association list). Respect +/– lower precedence
   than */ and parentheses. Variables are looked up in env.
 *)
let eval (env : (string * int) list) (expr : string list) : int =
  let rec eval_expr toks =
    let lhs, toks = eval_term toks in
    eval_expr_tail lhs toks
  and eval_expr_tail lhs toks =
    match toks with
    | "+" :: rest ->
      let rhs, rest = eval_term rest in
      eval_expr_tail (lhs + rhs) rest
    | "-" :: rest ->
      let rhs, rest = eval_term rest in
      eval_expr_tail (lhs - rhs) rest
    | _ -> lhs, toks
  and eval_term toks =
    let lhs, toks = eval_factor toks in
    eval_term_tail lhs toks
  and eval_term_tail lhs toks =
    match toks with
    | "*" :: rest ->
      let rhs, rest = eval_factor rest in
      eval_term_tail (lhs * rhs) rest
    | "/" :: rest ->
      let rhs, rest = eval_factor rest in
      eval_term_tail (lhs / rhs) rest
    | _ -> lhs, toks
  and eval_factor toks =
    match toks with
    | "(" :: rest ->
      let v, rest = eval_expr rest in
      (match rest with
       | ")" :: rest -> v, rest
       | _ -> assert false)
    | tok :: rest ->
      if all_upper tok then List.assoc tok env, rest
      else int_of_string tok, rest
    | [] -> assert false
  in
  let v, rest = eval_expr expr in
  match rest with
  | [] -> v
  | _ -> assert false

(* insert_uniq : 'k -> 'v -> ('k * 'v) list -> ('k * 'v) list
   Insert or replace a key/value in an asociation list ensuring
   the key appears at most once. Til-recursive via accumulator.
*)
let insert_uniq (k : 'k) (v : 'v) (r : ('k * 'v) list) : ('k * 'v) list =
  let rec go acc rem =
    match rem with
    | [] -> (k, v) :: acc
    | (k', _) :: rest when k = k' -> go acc rest
    | pair :: rest -> go (pair :: acc) rest
  in
  go [] r

let interp (input : string) (env : (string * int) list) : int * (string * int) list =
  match lex input with
  | var :: "=" :: expr -> (
    match eval env expr with
    | ouput -> ouput, insert_uniq var ouput env
    | exception _ -> failwith "whoops!"
  )
  | _
  | exception _ -> failwith "whoops!"
