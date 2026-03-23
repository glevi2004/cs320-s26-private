
let map (f : 'a -> 'b) (l : 'a list) : 'a list =
  let rec go acc = function
    | [] -> List.rev acc
    | x :: xs -> go (f x :: acc) xs
  in go [] l

let filter (f : 'a -> bool) (l : 'a list) : 'a list =
  let rec go acc = function
    | [] -> List.rev acc
    | x :: xs ->
      if f x
      then go (x :: acc) xs
      else go acc xs
  in go [] l

let fold_left
    (f : 'acc -> 'a -> 'acc)
    (base : 'acc)
    (l : 'a list) : 'acc =
  let rec go acc = function
    | [] -> acc
    | x :: xs -> go (f acc x) xs
  in go base l

let fold_right
    (f : 'a -> 'acc -> 'acc)
    (l : 'a list)
    (base : 'acc) : 'acc =
  fold_left (fun x y -> f y x) base l

let init (n : int) (f : int -> 'a) : 'a list =
  let rec go acc n =
    if n <= 0
    then acc
    else go (f n :: acc) (n - 1)
  in go [] n

module Matrix = struct
  type 'a t = 'a list list

  let init (dim : int * int) (f : int -> int -> 'a) : 'a t =
    let (r, c) = dim in
    let rec go acc n =
      if n < 0
      then acc
      else go (List.init c (f n) :: acc) (n - 1)
    in go [] (r - 1)

  let get (a : 'a t) (m : int) (n : int) : 'a option =
    match List.nth_opt a m with
    | Some r -> List.nth_opt r n
    | None -> None

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    let rec go acc = function
      | [] -> List.rev acc
      | r :: rs -> go (map f r :: acc) rs
    in go [] m

  let fold_left
      (f : 'acc -> 'a -> 'acc)
      (base : 'acc)
      (m : 'a t) : 'acc =
    let rec go acc m =
      match m with
      | [] -> acc
      | r :: rs -> go (List.fold_left f acc r) rs
    in go base m
end
