
let sqrt (n : int) : int =
  let rec loop i =
    if i * i >= n
    then i
    else loop (i + 1)
  in loop 0

let pow (n : int) (k : int) : int =
  let rec loop acc k =
    if k = 0
    then acc
    else loop (acc * n) (k - 1)
  in
  if k < 0
  then
    if abs n = 1
    then loop 1 (abs k)
    else failwith "whoops!"
  else
    loop 1 k

let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let string_of_char (c : char) : string =
  String.init 1 (fun _ -> c)

let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then List.rev acc
    else loop (String.get s i :: acc) (i + 1)
  in loop [] 0

let implode (cs : char list) : string =
  String.init
    (List.length cs)
    (fun i -> List.nth cs i)

let implode_all (css : char list list) : string list =
  let rec loop acc css =
    match css with
    | [] -> List.rev acc
    | cs :: rest -> loop (implode cs :: acc) rest
  in loop [] css

let split_on_ws_helper (cs : char list) : char list list =
  let rec go cs =
    match cs with
    | [] -> []
    | [c] -> if is_ws c then [] else [[c]]
    | c1 :: c2 :: cs ->
      if is_ws c1
      then go (c2 :: cs)
      else if is_ws c2
      then [c1] :: go cs
      else
        match go (c2 :: cs) with
        | [] -> assert false
        | w :: ws -> (c1 :: w) :: ws
  in go cs

let split_on_ws (s : string) : string list =
  implode_all (split_on_ws_helper (explode s))

let rec eval (stack : int list) (input : string list) : int list =
  match stack, input with
  | a :: b :: stack, "+" :: input -> eval (b + a :: stack) input
  | a :: b :: stack, "-" :: input -> eval (b - a :: stack) input
  | a :: b :: stack, "*" :: input -> eval (b * a :: stack) input
  | a :: b :: stack, "/" :: input -> eval (b / a :: stack) input
  | a :: b :: stack, "mod" :: input -> eval (b mod a :: stack) input
  | a :: stack, "sqrt" :: input -> eval (sqrt a :: stack) input
  | a :: b :: stack, "^" :: input -> eval (pow b a :: stack) input
  | stack, n :: input -> eval (int_of_string n :: stack) input
  | stack, [] -> stack

let interp (input : string) : int =
  match eval [] (split_on_ws input) with
  | [output] -> output
  | _ -> failwith "whoops!"
