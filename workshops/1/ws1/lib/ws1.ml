
let int_of_digits (digits : int list) : int =
  let rec loop acc digits =
    match digits with
    | [] -> acc
    | d :: ds -> loop (acc * 10 + d) ds
  in loop 0 (List.rev digits)

let digits_of_int (n : int) : int list =
  let rec loop acc n =
    if n = 0
    then List.rev acc
    else loop (n mod 10 :: acc) (n / 10)
  in loop [] n

let rec gcd (i : int) (j : int) : int =
  if i > j
  then gcd (i - j) j
  else if i < j
  then gcd i (j - i)
  else i

let py_triples (n : int) : (int * int * int) list =
  let rec loop acc i j k =
    if i > n
    then List.rev acc
    else if j > n
    then loop acc (i + 1) (i + 1) (i + 1)
    else if k > n
    then loop acc i (j + 1) (j + 1)
    else
      let acc =
        if i * i + j * j = k * k && gcd i (gcd j k) = 1
        then (i, j, k) :: acc
        else acc
      in loop acc i j (k + 1)
  in loop [] 1 1 1
