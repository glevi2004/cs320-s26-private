
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

(* Helper function to drop the last element of a list *)
let rec drop_last (l : string list) : string list =
  match l with
  | x :: y :: rest -> x :: drop_last (y :: rest)
  | _ -> []

(* Helper function to find the psition of a matching closing parenthesis *)
let rec find_matching_paren (tokens : string list) (depth : int) (pos : int) : int =
  match tokens with
  | [] -> pos
  | "(" :: rest -> find_matching_paren rest (depth + 1) (pos + 1)
  | ")" :: rest ->
    if depth = 1 then pos
    else find_matching_paren rest (depth - 1) (pos + 1)
  | _ :: rest -> find_matching_paren rest depth (pos + 1)

(* Helper function to take the first n elements of a list *)
let rec take (n : int) (l : string list) : string list =
  if n <= 0 then []
  else match l with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

(* Helper function to drop the first n elements of a list *)
let rec drop (n : int) (l : string list) : string list =
  if n <= 0 then l
  else match l with
    | [] -> []
    | _ :: xs -> drop (n - 1) xs

(* Split expression by + or - at the top level (respecting parentheses) *)
let split_add_sub (tokens : string list) : (string list * string * string list) option =
  let rec find_split_point (before : string list) (remaining : string list) (depth : int) 
      : (string list * string * string list) option =
    match remaining with
    | [] -> None
    | "(" :: rest -> find_split_point (before @ ["("]) rest (depth + 1)
    | ")" :: rest -> find_split_point (before @ [")"]) rest (depth - 1)
    | op :: rest when depth = 0 && (op = "+" || op = "-") ->
      (* Found a top-level + or - operator *)
      (* We want to find the LAST such operator for left associativity *)
      (match find_split_point (before @ [op]) rest depth with
       | Some result -> Some result
       | None -> Some (before, op, rest))
    | x :: rest -> find_split_point (before @ [x]) rest depth
  in
  find_split_point [] tokens 0

(* Split expression by * or / at the top level (respecting parentheses) *)
let split_mul_div (tokens : string list) : (string list * string * string list) option =
  let rec find_split_point (before : string list) (remaining : string list) (depth : int)
      : (string list * string * string list) option =
    match remaining with
    | [] -> None
    | "(" :: rest -> find_split_point (before @ ["("]) rest (depth + 1)
    | ")" :: rest -> find_split_point (before @ [")"]) rest (depth - 1)
    | op :: rest when depth = 0 && (op = "*" || op = "/") ->
      (* Found a top-level * or / operator *)
      (* want to find the LAST such operator for left associativity *)
      (match find_split_point (before @ [op]) rest depth with
       | Some result -> Some result
       | None -> Some (before, op, rest))
    | x :: rest -> find_split_point (before @ [x]) rest depth
  in
  find_split_point [] tokens 0

(* Mutually recursive functions *)
(* eval: handles + and - (lowest precedence) *)
let rec eval (expr : string list) : int =
  match split_add_sub expr with
  | Some (left, "+", right) -> eval left + eval_mul_div right
  | Some (left, "-", right) -> eval left - eval_mul_div right
  | Some _ -> assert false (* should not happen *)
  | None -> eval_mul_div expr

(* eval_mul_div: handles * and / (higher precedence) *)
and eval_mul_div (expr : string list) : int =
  match split_mul_div expr with
  | Some (left, "*", right) -> eval_mul_div left * eval_num_paren right
  | Some (left, "/", right) -> eval_mul_div left / eval_num_paren right
  | Some _ -> assert false (* should not happen *)
  | None -> eval_num_paren expr

(* eval_num_paren: handles numbers and parenthesized expressions *)
and eval_num_paren (expr : string list) : int =
  match expr with
  | [n] -> int_of_string n              (* number *)
  | "(" :: rest -> eval (drop_last rest) (* parenthesized expression *)
  | _ -> assert false                    (* undefined *)

let interp (input : string) : int =
  match eval (lex input) with
  | output -> output
  | exception _ -> failwith "whoops!"
