
(* Problem 1 *)

type 'a tree =
  | Leaf of 'a
  | Node2 of 'a * 'a tree * 'a tree
  | Node3 of 'a * 'a tree * 'a tree * 'a tree

let rec reverse (t : 'a tree) : 'a tree =
  match t with
  | Leaf x -> Leaf x
  | Node2 (x, l, r) -> Node2 (x, reverse r, reverse l)
  | Node3 (x, l, m, r) -> Node3 (x, reverse r, reverse m, reverse l)

(* Problem 2 *)

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let split_at (k : int) (s : string) : string * string =
  if k < 0
  then "", s
  else if k > String.length s
  then s, ""
  else String.sub s 0 k, String.sub s k (String.length s - k)

let get_int (s : string) : (int * string) option =
  let rec loop i =
    if i >= String.length s
    then (int_of_string s, "")
    else if is_digit s.[i]
    then loop (i + 1)
    else
      let (n, s) = split_at i s in
      int_of_string n, s

  in
  if is_digit s.[0]
  then Some (loop 1)
  else if s.[0] = '-' && is_digit s.[1]
  then Some (loop 2)
  else None

(* Problem 3 *)

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Exp of expr * expr

type error =
  | DivByZero
  | NegExp

let int_pow m n =
  let rec go acc i =
    if i <= 0
    then acc
    else go (m * acc) (i - 1)
  in go 1 n

let rec eval (e : expr) : (int, error) result =
  match e with
  | Int n -> Ok n
  | Add (e1, e2) -> (
      match eval e1, eval e2 with
      | Ok m, Ok n -> Ok (m + n)
      | Error e, _ -> Error e
      | _, Error e -> Error e
    )
  | Sub (e1, e2) -> (
      match eval e1, eval e2 with
      | Ok m, Ok n -> Ok (m - n)
      | Error e, _ -> Error e
      | _, Error e -> Error e
    )
  | Mul (e1, e2) -> (
      match eval e1, eval e2 with
      | Ok m, Ok n -> Ok (m * n)
      | Error e, _ -> Error e
      | _, Error e -> Error e
    )
  | Div (e1, e2) -> (
      match eval e1, eval e2 with
      | Ok _, Ok 0 -> Error DivByZero
      | Ok m, Ok n -> Ok (m / n)
      | Error e, _ -> Error e
      | _, Error e -> Error e
    )
  | Exp (e1, e2) -> (
      match eval e1, eval e2 with
      | Ok m, Ok n ->
        if n < 0
        then Error NegExp
        else Ok (int_pow m n)
      | Error e, _ -> Error e
      | _, Error e -> Error e
    )

(* Problem 4 *)

type content_type =
  | PlainText
  | Html
  | Pdf
  | Png

type content_encoding =
  | Base64
  | QuotePrintable
  | Binary

type header =
  {
    content_type : content_type;
    content_encoding : content_encoding;
  }

type 'a email =
  | Attachment of header * 'a
  | Body of header * string
  | Multipart of (header * 'a email option) list

let get_attachments (t : content_type) (e : 'a email) : 'a list =
  let rec go e =
    match e with
    | Attachment (h, d) ->
      if h.content_type = t
      then [d]
      else []
    | Body _ -> []
    | Multipart parts ->
      let rec loop acc es =
        match es with
        | [] -> acc
        | (_, None) :: es -> loop acc es
        | (_, Some e) :: es -> loop (List.rev_append (go e) acc) es
      in loop [] parts
  in go e
