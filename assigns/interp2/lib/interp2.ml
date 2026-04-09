open Utils
module Error_msg = Error_msg

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Interp2.ty =
    | TUnit
    | TBool
    | TInt
    | TInt_list
    | TFun of ty * ty
    | TTuple of ty list

let rec pp_ty ppf ty =
  let open Fmt in
  let pp_parens ppf ty =
    match ty with
    | TFun (_, _)
    | TTuple _
    | _ -> pp_ty ppf ty
  in
  match ty with
  | TUnit -> pf ppf "unit"
  | TBool -> pf ppf "bool"
  | TInt -> pf ppf "int"
  | TFun (t1, t2) -> pf ppf "%a -> %a" pp_parens t1 pp_ty t2
  | TTuple ts -> list ~sep:(Fmt.any " * ") pp_ty ppf ts
  | TInt_list -> pf ppf "int list"

type _pattern = Ast.Interp2._pattern =
  | PUnit
  | PBool of bool
  | PInt of int
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list
  | PVar of string
and pattern = Ast.Interp2.pattern =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Interp2.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or | Cons

type _expr = Ast.Interp2._expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Nil
  | Assert of expr
  | Negate of expr
  | Tuple of expr list
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Fun of (string * ty) list * expr
  | App of expr * expr list
  | Let of
      {
        is_rec : bool;
        name : string;
        args : (string * ty) list;
        annot : ty option;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Interp2.expr =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Interp2._stmt =
  | SLet of {
      is_rec : bool;
      name : string;
      args : (string * ty) list;
      annot : ty option;
      binding : expr;
    }
and stmt = Ast.Interp2.stmt =
  {
    pos : pos;
    stmt : _stmt;
  }

type prog = stmt list

module Env = Map.Make(String)

(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_tuple_pat pos t =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of type %a"
      pp_ty t
  in Error_msg.mk pos msg

let exp_diff_tuple_pat pos ty =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of a different tuple type %a"
      pp_ty ty
  in Error_msg.mk pos msg

let not_func pos ty =
  let msg =
    Format.asprintf
      "This expression has type %a. This is not a function; it cannot be applied"
      pp_ty ty
  in Error_msg.mk pos msg

let too_many_args pos ty =
  let msg =
    Format.asprintf
      "This function has type %a. It is applied to to many arguments"
      pp_ty ty
  in Error_msg.mk pos msg

let missing_rec_annot pos =
  Error_msg.mk pos "Must provide output type annotation for recursive function"

let missing_rec_arg pos =
  Error_msg.mk pos "Must provide argument for recursive function"

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg


(* TYPING
   ----------------------------------------------------------------------
*)

(* Contexts *)

type ctxt = ty Env.t

(* Type Checking *)

let type_of_expr (ctxt : ctxt) (e : expr) : (ty, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let rec go (ctxt : ctxt) (e : expr) : (ty, Error_msg.t) result =
    match e.expr with
    | Unit -> Ok TUnit
    | Bool _ -> Ok TBool
    | Int _ -> Ok TInt
    | Var x ->
      (match Env.find_opt x ctxt with
       | Some ty -> Ok ty
       | None -> Error (unknown_var e.pos x))
    | Nil -> Ok TInt_list
    | Assert e1 ->
      let* _ = go ctxt e1 in
      Ok TUnit
    | Negate e1 ->
      let* _ = go ctxt e1 in
      Ok TInt
    | Tuple es ->
      let* tys = map_ok (go ctxt) es in
      Ok (TTuple tys)
    | Bop (op, e1, e2) ->
      let* _ = go ctxt e1 in
      let* _ = go ctxt e2 in
      (match op with
       | Add | Sub | Mul | Div | Mod -> Ok TInt
       | Eq | Neq | Lt | Lte | Gt | Gte -> Ok TBool
       | And | Or -> Ok TBool
       | Cons -> Ok TInt_list)
    | If (e1, e2, e3) ->
      let* _ = go ctxt e1 in
      let* t2 = go ctxt e2 in
      let* _ = go ctxt e3 in
      Ok t2
    | Fun (args, body) ->
      let ctxt' = List.fold_left (fun c (x, ty) -> Env.add x ty c) ctxt args in
      let* ret_ty = go ctxt' body in
      Ok (List.fold_right (fun (_, ty) acc -> TFun (ty, acc)) args ret_ty)
    | App (f_expr, arg_exprs) ->
      let* f_ty = go ctxt f_expr in
      let* _ = map_ok (go ctxt) arg_exprs in
      let rec peel ty n =
        if n = 0 then Ok ty
        else match ty with
          | TFun (_, ret) -> peel ret (n - 1)
          | _ -> assert false
      in
      peel f_ty (List.length arg_exprs)
    | Let {is_rec; name; args; annot; binding; body} ->
      (match is_rec, args, annot with
       | _, [], _ ->
         let* bind_ty = go ctxt binding in
         go (Env.add name bind_ty ctxt) body
       | false, _, _ ->
         let ctxt' = List.fold_left (fun c (x, ty) -> Env.add x ty c) ctxt args in
         let* ret_ty = go ctxt' binding in
         let fun_ty = List.fold_right (fun (_, ty) acc -> TFun (ty, acc)) args ret_ty in
         go (Env.add name fun_ty ctxt) body
       | true, _, Some ann_ty ->
         let fun_ty = List.fold_right (fun (_, ty) acc -> TFun (ty, acc)) args ann_ty in
         let ctxt' = List.fold_left (fun c (x, ty) -> Env.add x ty c) (Env.add name fun_ty ctxt) args in
         let* _ = go ctxt' binding in
         go (Env.add name fun_ty ctxt) body
       | true, _, None -> assert false)
    | Match _ -> assert false
  in
  go ctxt e

let type_of (p : prog) : (ty, Error_msg.t) result =
  let rec go ctxt ty p =
    match p with
    | [] -> Ok (Option.value ~default:TUnit ty)
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps -> (
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      match type_of_expr ctxt e with
      | Ok ty ->
        let ctxt = Env.add name ty ctxt in
        go ctxt (Some ty) ps
      | Error err -> Error err
    )
  in go Env.empty None p


(* EVALUATION
   ----------------------------------------------------------------------
*)

(* Values *)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      args : string list;
      body : expr;
    }
  | VInt_list of int list

(* Dynamic Environments *)

type dyn_env = value Env.t

(* Evaluation *)

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos

let eval_expr (env : dyn_env) (e : expr) : value =
  let rec go (env : dyn_env) (e : expr) : value =
    match e.expr with
    | Unit -> VUnit
    | Bool b -> VBool b
    | Int n -> VInt n
    | Var x -> Env.find x env
    | Nil -> VInt_list []
    | Assert e1 ->
      (match go env e1 with
       | VBool true -> VUnit
       | _ -> raise (Assert_fail e.pos))
    | Negate e1 ->
      (match go env e1 with
       | VInt n -> VInt (-n)
       | _ -> assert false)
    | Tuple es -> VTuple (List.map (go env) es)
    | Bop (op, e1, e2) -> eval_bop env e.pos op e1 e2
    | If (e1, e2, e3) ->
      (match go env e1 with
       | VBool true -> go env e2
       | VBool false -> go env e3
       | _ -> assert false)
    | Fun (args, body) ->
      VClos {env; name=None; args=List.map fst args; body}
    | App (f_expr, arg_exprs) ->
      let f_val = go env f_expr in
      let arg_vals = List.map (go env) arg_exprs in
      List.fold_left apply f_val arg_vals
    | Let {is_rec; name; args; binding; body; _} ->
      (match args with
       | [] ->
         let v = go env binding in
         go (Env.add name v env) body
       | _ ->
         let name_opt = if is_rec then Some name else None in
         let closure = VClos {env; name=name_opt; args=List.map fst args; body=binding} in
         go (Env.add name closure env) body)
    | Match _ -> assert false
  and apply (f_val : value) (arg_val : value) : value =
    match f_val with
    | VClos {env=clos_env; name; args; body} ->
      let env' = match name with
        | Some f -> Env.add f f_val clos_env
        | None -> clos_env
      in
      (match args with
       | [x] -> go (Env.add x arg_val env') body
       | x :: rest -> VClos {env=Env.add x arg_val env'; name=None; args=rest; body}
       | [] -> assert false)
    | _ -> assert false
  and eval_bop env pos op e1 e2 =
    match op with
    | And ->
      (match go env e1 with
       | VBool true -> go env e2
       | v -> v)
    | Or ->
      (match go env e1 with
       | VBool false -> go env e2
       | v -> v)
    | _ ->
      let v1 = go env e1 in
      let v2 = go env e2 in
      (match op, v1, v2 with
       | Add, VInt a, VInt b -> VInt (a + b)
       | Sub, VInt a, VInt b -> VInt (a - b)
       | Mul, VInt a, VInt b -> VInt (a * b)
       | Div, VInt _, VInt 0 -> raise (Div_by_zero pos)
       | Div, VInt a, VInt b -> VInt (a / b)
       | Mod, VInt _, VInt 0 -> raise (Div_by_zero pos)
       | Mod, VInt a, VInt b -> VInt (a mod b)
       | Eq, _, _ -> VBool (v1 = v2)
       | Neq, _, _ -> VBool (v1 <> v2)
       | Lt, _, _ -> VBool (v1 < v2)
       | Lte, _, _ -> VBool (v1 <= v2)
       | Gt, _, _ -> VBool (v1 > v2)
       | Gte, _, _ -> VBool (v1 >= v2)
       | Cons, VInt n, VInt_list ns -> VInt_list (n :: ns)
       | _ -> assert false)
  in
  go env e

let eval (p : prog) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
  in go Env.empty None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value * ty, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* prog = Ast.Interp2.prog_of_prog prog in
  let* ty = type_of prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
  in
  Ok (v, ty)


(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _ = " ^ s in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false

let parse_ty s =
  let s = "let _ : " ^ s ^ " = assert false" in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {annot=Some ty;_}}] -> ty
  | _ -> assert false
