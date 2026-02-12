
let rec split_at (k : int) (l : 'a list) : 'a list * 'a list =
  match k , l with
  | _, [] -> [], []
  | 0, l -> [], l
  | k, x :: xs ->
    let (lhs, rhs) = split_at (k - 1) xs in
    (x :: lhs, rhs)

(*
    s 3 [1;2;3;4;5] ---> [1;2;3], [4;5]
    s 2 [2;3;4;5]   ---> [2;3], [4;5]
    s 1 [3;4;5]     ---> [3], [4;5]
    s 0 [4;5]       ---> [], [4;5]
*)

type 'a matrix =
  {
    rows : int;
    cols : int;
    entries : 'a list list;
  }

type error =
  | RowMismatch
  | ColMismatch

let rec all_len_equal
    (n : int)
    (l : 'a list list) : bool =
  match l with
  | [] -> true
  | x :: xs -> List.length x = n && all_len_equal n xs

let mk_matrix
    (rows : int)
    (cols : int)
    (entries : 'a list list) : ('a matrix, error) result =
  if List.length entries <> rows
  then Error RowMismatch
  else if all_len_equal cols entries
  then
    Ok {
      rows = rows;
      cols = cols;
      entries = entries;
    }
  else Error ColMismatch
