
(* Problem 1 *)

type 'a tree =
  | Leaf of 'a
  | Node2 of 'a * 'a tree * 'a tree
  | Node3 of 'a * 'a tree * 'a tree * 'a tree

let reverse (_t : 'a tree) : 'a tree =
  assert false

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

let get_int (_s : string) : (int * string) option =
  assert false

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

let eval (_e : expr) : (int, error) result =
  assert false

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

let get_attachments (_t : content_type) (_e : 'a email) : 'a list =
  assert false
