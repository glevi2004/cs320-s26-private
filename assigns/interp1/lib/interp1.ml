
(* Syntax *)

type ty = Ast.Interp1.ty =
  | Unit
  | Bool
  | Int
  | Fun of ty * ty

type bop = Ast.Interp1.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

type expr = Ast.Interp1.expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Let of string * expr * expr
  | LetRec of {
      name : string;
      arg : string;
      arg_ty : ty;
      out_ty : ty;
      binding : expr;
      body : expr;
    }
  | If of expr * expr * expr
  | Fun of string * ty * expr
  | App of expr * expr
  | Bop of bop * expr * expr
  | Negate of expr
  | Assert of expr

(* Environments *)

module Env = Map.Make (String)

(* Values *)

type value =
  | Unit
  | Bool of bool
  | Int of int
  | Clos of value Env.t * string option * expr

(* Contexts *)

type ctxt = ty Env.t

(* Dynamic Environments *)

type dyn_env = value Env.t

(* Type Checking *)

let rec type_of (ctxt : ctxt) (e : expr) : ty option =
  match e with
  | Unit -> Some Unit
  | Bool _ -> Some Bool
  | Int _ -> Some Int
  | Var x -> Env.find_opt x ctxt
  | Assert e ->
    (match type_of ctxt e with
     | Some Bool -> Some Unit
     | _ -> None)
  | Negate e ->
    (match type_of ctxt e with
     | Some Int -> Some Int
     | _ -> None)
  | Bop (op, e1, e2) ->
    (match type_of ctxt e1, type_of ctxt e2 with
     | Some t1, Some t2 ->
       (match op with
        | Add | Sub | Mul | Div | Mod ->
          (match t1, t2 with
           | Int, Int -> Some Int
           | _ -> None)
        | Eq | Neq | Lt | Lte | Gt | Gte ->
          if t1 = t2 then Some Bool else None
        | And | Or ->
          (match t1, t2 with
           | Bool, Bool -> Some Bool
           | _ -> None))
     | _ -> None)
  | Fun (x, t1, body) ->
    let ctxt' = Env.add x t1 ctxt in
    (match type_of ctxt' body with
     | Some t2 -> Some (Fun (t1, t2) : ty)
     | None -> None)
  | App (e1, e2) ->
    (match type_of ctxt e1, type_of ctxt e2 with
     | Some (Fun (t_arg, t_ret) : ty), Some t2 ->
       if t_arg = t2 then Some t_ret else None
     | _ -> None)
  | If (e1, e2, e3) ->
    (match type_of ctxt e1, type_of ctxt e2, type_of ctxt e3 with
     | Some Bool, Some t2, Some t3 ->
       if t2 = t3 then Some t2 else None
     | _ -> None)
  | Let (x, e1, e2) ->
    (match type_of ctxt e1 with
     | Some t1 ->
       let ctxt' = Env.add x t1 ctxt in
       type_of ctxt' e2
     | None -> None)
  | LetRec { name; arg; arg_ty; out_ty; binding; body } ->
    let fun_ty : ty = Fun (arg_ty, out_ty) in
    let ctxt_binding = Env.add arg arg_ty (Env.add name fun_ty ctxt) in
    (match type_of ctxt_binding binding with
     | Some t when t = out_ty ->
       let ctxt_body = Env.add name fun_ty ctxt in
       type_of ctxt_body body
     | _ -> None)

(* Evaluation *)

exception Div_by_zero
exception Assert_fail

let rec eval (env : dyn_env) (e : expr) : value =
  match e with
  | Unit -> Unit
  | Bool b -> Bool b
  | Int n -> Int n
  | Var x -> Env.find x env
  | Assert e ->
    (match eval env e with
     | Bool true -> Unit
     | Bool false -> raise Assert_fail
     | _ -> assert false)
  | Negate e ->
    (match eval env e with
     | Int n -> Int (-n)
     | _ -> assert false)
  | Bop (op, e1, e2) ->
    let eval_int e =
      match eval env e with
      | Int n -> n
      | _ -> assert false
    in
    (match op with
     | Add -> Int (eval_int e1 + eval_int e2)
     | Sub -> Int (eval_int e1 - eval_int e2)
     | Mul -> Int (eval_int e1 * eval_int e2)
     | Div ->
       let v2 = eval_int e2 in
       if v2 = 0 then raise Div_by_zero
       else Int (eval_int e1 / v2)
     | Mod ->
       let v2 = eval_int e2 in
       if v2 = 0 then raise Div_by_zero
       else Int (eval_int e1 mod v2)
     | Eq -> Bool (eval env e1 = eval env e2)
     | Neq -> Bool (eval env e1 <> eval env e2)
     | Lt -> Bool (eval env e1 < eval env e2)
     | Lte -> Bool (eval env e1 <= eval env e2)
     | Gt -> Bool (eval env e1 > eval env e2)
     | Gte -> Bool (eval env e1 >= eval env e2)
     | And ->
       (match eval env e1 with
        | Bool true -> eval env e2
        | Bool false -> Bool false
        | _ -> assert false)
     | Or ->
       (match eval env e1 with
        | Bool true -> Bool true
        | Bool false -> eval env e2
        | _ -> assert false))
  | Fun (x, t, body) ->
    Clos (env, None, Fun (x, t, body))
  | App (e1, e2) ->
    let clos = eval env e1 in
    let v2 = eval env e2 in
    (match clos with
     | Clos (clos_env, name_opt, Fun (x, _, body)) ->
       let env' = match name_opt with
         | None -> Env.add x v2 clos_env
         | Some f -> Env.add x v2 (Env.add f clos clos_env)
       in
       eval env' body
     | _ -> assert false)
  | If (e1, e2, e3) ->
    (match eval env e1 with
     | Bool true -> eval env e2
     | Bool false -> eval env e3
     | _ -> assert false)
  | Let (x, e1, e2) ->
    let v1 = eval env e1 in
    let env' = Env.add x v1 env in
    eval env' e2
  | LetRec { name; arg; arg_ty; out_ty = _; binding; body } ->
    let clos = Clos (env, Some name, Fun (arg, arg_ty, binding)) in
    let env' = Env.add name clos env in
    eval env' body

(* Interpretation *)

let interp ~(filename : string) : value option =
  let e_ty =
    match Syntax.parse ~filename with
    | Ok p -> Ast.Interp1.expr_of_prog p
    | Error e -> Error e
  in
  match e_ty with
  | Ok e -> (
      match type_of Env.empty e with
      | Some _ -> Some (eval Env.empty e)
      | _ ->
        let _type_error_msg = print_endline "Type error"
        in None
    )
  | Error e ->
    let _parse_error_msg =
      In_channel.with_open_text filename
        (fun ic ->
           let text = In_channel.input_all ic in
           let msg = Error_msg.to_string ~filename ~text e in
           Format.eprintf "%s" msg)
    in None
