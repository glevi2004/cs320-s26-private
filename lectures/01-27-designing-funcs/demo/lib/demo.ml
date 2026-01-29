
let rec digits_of_int (n : int) : int list =
  if n = 0
  then []
  else (n mod 10) :: digits_of_int (n / 10)

let int_of_digits (l : int list) : int =
  let rec loop n l =
    match l with
    | [] -> n
    | x :: xs -> loop (10 * n + x) xs
  in loop 0 (List.rev l)

(*
   int_of_digits [3;2;1]
   loop 0 [3;2;1]
   loop (10 * 0 + 3) [2;1]
   loop 3 [2;1]
   loop (10 * 3 + 2) [1]
   loop 32 [1]
   loop (10 * 32 + 1) []
   loop 321 []
   321
   *)


  (*
     n = 0
     l = [3;2;1]

     while l is not empty:
         n = 10 * n + l[0]
         l = l[1:]

     return n

  (*
     n = 0
     i = 0
     l = [3;2;1]

     while l is not empty:
         n = n + (10 ^ i) * l[0]
         i = i + 1
         l = l[1:]

     return n
     *)

     *)



  (* match l with *)
  (* | [] -> 0 *)
  (* | first_digit :: other_digits -> *)
  (*   first_digit + int_of_digits other_digits * 10 *)

(*
   int_of_digits [3;2;1]
   3 + int_of_digits [2;1] * 10
   3 + (2 + int_of_digits [1] * 10) * 10
   3 + (2 + (1 + int_of_digits [] * 10) * 10) * 10
   3 + (2 + (1 + 0 * 10) * 10) * 10
   3 + (2 + (1 * 10)) * 10
   3 + (2 + 10) * 10
   3 + 12 * 10
   3 + 120
   123

   *)
