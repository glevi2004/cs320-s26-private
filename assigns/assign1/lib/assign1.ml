(* sqrt : int -> int
   Returns the smallest integer k such that n <= k * k
   
   Approach: Simple recursion,increment k from 0 until k*k >= n
   Concept: Recursive helper function pattern (Lecture 2: Recursion and Lists) *)
let sqrt (n : int) : int =
  let rec loop (k : int) : int =
    if n <= k * k
    then k
    else loop (k + 1)
  in loop 0

(* pow : int -> int -> int
   Returns m to the power of n
   
   Approach: Tail recursion with accumulator, multiply acc by m, decrement exp
   Concept: accumulator pattern for tail recursion (Lecture 2: Tail Recursion) *)
let pow (m : int) (n : int) : int =
  let rec loop (acc : int) (exp : int) : int =
    if exp <= 0
    then acc
    else loop (acc * m) (exp - 1)
  in loop 1 n

(* is_ws : char -> bool
   Returns true if the character is whitespace *)
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

(* string_of_char : char -> string
   Converts a single character to a string *)
let string_of_char (c : char) : string =
  String.init 1 (fun _ -> c)

(* explode : string -> char list
   Converts a string to a list of characters *)
let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then List.rev acc
    else loop (String.get s i :: acc) (i + 1)
  in loop [] 0

(* implode : char list -> string
   Converts a list of characters back to a string *)
let implode (cs : char list) : string =
  String.init
    (List.length cs)
    (fun i -> List.nth cs i)

(* implode_all : char list list -> string list
   Converts a list of character lists to a list of strings *)
let implode_all (css : char list list) : string list =
  let rec loop acc css =
    match css with
    | [] -> List.rev acc
    | cs :: rest -> loop (implode cs :: acc) rest
  in loop [] css

(* split_on_ws_helper : char list -> char list list
   Splits a character list into sublists separated by whitespace
   
   Approach: Two accumulators, 'current' builds each word, 'result' collects words
   Concept: Accumulator pattern with List.rev for correct ordering 
            (Lecture 2: Tail Recursion and Lists) *)
let split_on_ws_helper (cs : char list) : char list list =
  let rec loop (result : char list list) (current : char list) (chars : char list) : char list list =
    match chars with
    | [] ->
      (* End of input: add current word if non-empty, then reverse result *)
      if current = []
      then List.rev result
      else List.rev (List.rev current :: result)
    | c :: rest ->
      if is_ws c
      then
        (* Whitespace: if we have a current word, add it to result *)
        if current = []
        then loop result [] rest
        else loop (List.rev current :: result) [] rest
      else
        (* Non-whitespace: add character to current word *)
        loop result (c :: current) rest
  in loop [] [] cs

(* split_on_ws : string -> string list
   Splits a string into a list of whitespace-separated substrings *)
let split_on_ws (s : string) : string list =
  implode_all (split_on_ws_helper (explode s))

(* eval : int list -> string list -> int list
   Evaluates an RPN expression with a starting stack
   
   Approach: Process tokens left-to-right using pattern matching
   - Integers: push onto stack
   - Operators: pop operand(s), compute, push result
   
   Concept: Stack-based evaluation, pattern matching on strings
            (Lecture 2: Pattern Matching on Lists) *)
let eval (stack : int list) (prog : string list) : int list =
  let rec loop (stk : int list) (tokens : string list) : int list =
    match tokens with
    | [] -> stk
    | token :: rest ->
      match token with
      | "+" ->
        (match stk with
         | b :: a :: stk' -> loop ((a + b) :: stk') rest
         | _ -> failwith "stack underflow")
      | "-" ->
        (match stk with
         | b :: a :: stk' -> loop ((a - b) :: stk') rest
         | _ -> failwith "stack underflow")
      | "*" ->
        (match stk with
         | b :: a :: stk' -> loop ((a * b) :: stk') rest
         | _ -> failwith "stack underflow")
      | "/" ->
        (match stk with
         | b :: a :: stk' -> loop ((a / b) :: stk') rest
         | _ -> failwith "stack underflow")
      | "mod" ->
        (match stk with
         | b :: a :: stk' -> loop ((a mod b) :: stk') rest
         | _ -> failwith "stack underflow")
      | "^" ->
        (match stk with
         | b :: a :: stk' -> loop (pow a b :: stk') rest
         | _ -> failwith "stack underflow")
      | "sqrt" ->
        (match stk with
         | a :: stk' -> loop (sqrt a :: stk') rest
         | _ -> failwith "stack underflow")
      | _ ->
        (* Must be an integer - push onto stack *)
        let n = int_of_string token in
        loop (n :: stk) rest
  in loop stack prog

(* interp : string -> int
   Interprets an RPN expression string and returns the result *)
let interp (input : string) : int =
  match eval [] (split_on_ws input) with
  | [output] -> output
  | _ -> failwith "whoops!"
