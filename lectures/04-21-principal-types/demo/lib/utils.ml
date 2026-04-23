
(* monotype *)
type ty =
  | TInt
  | TBool
  | TFun of ty * ty
  | TPar of string

type ty_scheme = string list * ty

type expr =
  | Var of string
  | Num of int
  | Fun of string * expr
  | Add of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of string * expr * expr

type prog = (string * expr) list

module Env = Map.Make(String)

type dyn_env = value Env.t
and value =
  | VBool of bool
  | VNum of int
  | VClos of string * expr * dyn_env

let string_of_ty_scheme (_, ty) =
  let rec go = function
    | TInt -> "int"
    | TBool -> "bool"
    | TPar a -> "\'" ^ a
    | TFun (t1, t2) -> go_paren t1 ^ " -> " ^ go t2
  and go_paren = function
    | TFun (t1, t2) -> "(" ^ go (TFun (t1, t2)) ^ ")"
    | ty -> go ty
  in
  go ty
