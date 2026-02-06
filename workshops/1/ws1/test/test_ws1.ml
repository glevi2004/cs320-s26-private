open Ws1

let _ = assert (int_of_digits [1;2;3] = 321)
let _ = assert (digits_of_int 123 = [3;2;1])

let _ = assert
  (py_triples 30
   = [(3, 4, 5); (5, 12, 13); (7, 24, 25); (8, 15, 17); (20, 21, 29)])
