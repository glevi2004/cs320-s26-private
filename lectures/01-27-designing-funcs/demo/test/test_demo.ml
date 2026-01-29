open Demo

let _ = assert (digits_of_int 123 = [3;2;1])
let _ = assert (digits_of_int 1235 = [5;3;2;1])
let _ = assert (int_of_digits [3;2;1] = 123)
