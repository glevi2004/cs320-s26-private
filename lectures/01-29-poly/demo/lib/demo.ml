
(*
   range i j then from j to k

   keep track of "direction"

   "natural" recursion
*)

(*

   b 2 4 2
   2 :: b 3 4 2
   2 :: 3 :: b 4 4 2

   *)


let rec bitonic i j k =
  (* if i = j && j = k *)
  (* then [i] *)
  (* else if i < j *)
  if i < j
  then i :: bitonic (i + 1) j k
  else if i > j
  then i :: bitonic (i - 1) j k
  else if j < k
  then j :: bitonic (i + 1) (j + 1) k
  else if j > k
  then j :: bitonic (i - 1) (j - 1) k
  else [k]

let range i j =
  let rec loop acc i j =
    if i < j
    then loop (j :: acc) i (j - 1)
    else if i > j
    then loop (j :: acc) i (j + 1)
    else i :: acc
  in loop [] i j

let bitonic2 i j k = range i j @ List.tl (range j k)

let rec remove x l =
  match l with
  | [] -> []
  | y :: ys ->
    if x = y
    then remove x ys
    else y :: remove x ys
