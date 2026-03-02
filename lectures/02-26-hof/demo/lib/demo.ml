
let range i j =
  let rec go acc i j =
    if j < i
    then acc
    else go (j :: acc) i (j - 1)
  in go [] i j

let fact n = List.fold_right ( * ) (range 0 n) 1
let sum n = List.fold_right (+) (range 0 n) 0

let string_of_ints n =
  List.fold_right
    (fun i s -> s ^ " " ^ string_of_int i)
    (range 0 n)
    ""

type vector = float list

let norm (v : vector) : float =
  v
  |> List.map (fun x -> x *. x)
  |> List.fold_left (+.) 0.
  |> sqrt

let normalize (v : vector) : vector =
  List.map (fun x -> x /. norm v) v


let primes (n : int) : int list =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | p :: xs ->
      go
        (p :: acc)
        (List.filter (fun x -> x mod p <> 0) xs)
  in go [] (range 2 n)
let fold_right op l base =
  let rec loop acc l =
    match l with
    | [] -> acc
    | x :: xs -> loop (op acc x) xs
  in loop base l


type op =
  | Add
  | Sub
  | Mul
  | Div

type expr =
  | Int of int
  | Bop of op * expr * expr

let map2 (f : 'a -> 'b -> 'c) (x : 'a option) (y : 'b option) : 'c option =
  match x, y with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let fun_of_op = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> ( * )
  | Div -> (/)

let rec eval (e : expr) : int option =
  match e with
  | Int n -> Some n
  | Bop (op, e1, e2) -> (
      match op with
      | Add | Sub | Mul -> map2 (fun_of_op op) (eval e1) (eval e2)
      | Div -> (
          match eval e2 with
          | Some 0 -> None
          | Some n -> Option.map (fun x -> x / n) (eval e1)
          | None -> None
        )
