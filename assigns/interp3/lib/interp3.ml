open Utils
module Error_msg = Error_msg
module Ast = Ast

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Type.t =
  | TUnit
  | TBool
  | TInt
  | TString
  | TTuple of ty list
  | TAdt of ty list * string
  | TFun of ty * ty
  | TParam of string

type _pattern = Ast.Pattern.pattern =
  | PWild
  | PVar of string
  | PUnit
  | PBool of bool
  | PInt of int
  | PString of string
  | PTuple of pattern list
  | PCons of string * pattern option
and pattern = Ast.Pattern.t =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Expr.bop =
  | Add | Sub | Mul
  | Div | Mod
  | And | Or
  | Concat
  | Eq | Neq | Lt | Lte | Gt | Gte

type _expr = Ast.Expr.expr =
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Negate of expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Annot of expr * ty
  | Tuple of expr list
  | Assert of expr
  | Var of string
  | Cons of string * expr option
  | Fun of (string * ty option) * expr
  | App of expr * expr
  | Let of
      {
        is_rec : bool;
        name : string;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Expr.t =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Stmt.stmt =
  | SLet of
      {
        is_rec : bool;
        name : string;
        binding : expr;
      }

  | SAdt of
      {
        tpars : string list;
        name : string;
        constrs : (string * ty option) list
      }
and stmt = Ast.Stmt.t =
  {
    pos : pos;
    stmt : _stmt;
  }

module Env = Map.Make(String)


(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let dummy_error = Error_msg.mk dummy_pos "Dummy error"

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let invalid_app pos = Error_msg.mk pos "Invalid application"

let invalid_tuple pos = Error_msg.mk pos "Invalid tuple"

let unknown_cons pos x = Error_msg.mk pos (Format.asprintf "Unbound constructor %s" x)

let cons_exp_no_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects 0 arguments" x)

let cons_exp_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects arguments" x)

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg

let dup_ty_name pos x =
  let msg =
    Format.asprintf
      "Type using name %s is already defined"
      x
  in Error_msg.mk pos msg

let unbound_ty_var pos n =
  Error_msg.mk
    pos
    (Format.asprintf "The type variable %s is unbound in this type declaration" n)

let ty_param_several_times pos =
  Error_msg.mk
    pos
    "A type parameter occurs several times"

(* TYPING
   ----------------------------------------------------------------------
*)

type ty_scheme = string list * ty
type ctxt = ty_scheme Env.t
type constr = ty * ty * (ty -> ty -> Error_msg.t)

(* raised during constraint generation for context-level errors like unbound vars/constructors and wrong constructor arity *)
exception Type_error of Error_msg.t

let fresh () = TParam (_gensym ())

(* subst, list of (type-var-name, type) pairs *)
type subst = (string * ty) list

(* Apply substitution s to a type. *)
let rec apply_ty (s : subst) (t : ty) : ty =
  let rec lookup a = function
    | [] -> TParam a
    | (b, t') :: _ when b = a -> t'
    | _ :: rest -> lookup a rest
  in
  match t with
  | TUnit | TBool | TInt | TString -> t
  | TTuple ts -> TTuple (List.map (apply_ty s) ts)
  | TAdt (ts, n) -> TAdt (List.map (apply_ty s) ts, n)
  | TFun (a, b) -> TFun (apply_ty s a, apply_ty s b)
  | TParam a -> lookup a s

(* Apply substitution to every type in a constraint list, keeping the erro msg function attached to each constraint *)
let apply_constrs (s : subst) (cs : constr list) : constr list =
  List.map (fun (x, y, f) -> apply_ty s x, apply_ty s y,  f) cs


(* Occurs check if type variable a appear inside t. Used by unification to avoid infinite types like a = a -> a. *)
let rec occurs (a : string)  (t : ty) : bool =
  match t with
  | TParam b -> a = b
  | TFun (x, y) -> occurs  a x || occurs a y
  | TTuple ts | TAdt (ts , _ ) -> List.exists (occurs a) ts
  | _ -> false


(* Unification, rules S, F, A from the principal types lecture.
   Each constraint carries its own error-msg function, used on failure. *)
let unify (cs : constr list) : (subst, Error_msg.t) result =
  let rec zip_with_f xs ys f =
    match xs, ys with
    | [], [] -> []
    | x :: xs', y :: ys' -> (x, y, f) :: zip_with_f xs' ys' f
    | _ -> []
  in
  let rec go (s : subst) (u : constr list) =
    match u with
    | [] -> Ok s
    (* S: syntactically equal, drop *)
    | (t1, t2, _) :: rest when t1 = t2 -> go s rest
    (* F: function types split into two equations *)
    | (TFun (a, b), TFun (c, d), f) :: rest ->
      go s ((a, c, f) :: (b, d, f) :: rest)
    (* F: tuples *)
    | (TTuple xs, TTuple ys, f) :: rest when List.length xs = List.length ys ->
      go s (zip_with_f xs ys f @ rest)
    (* F: ADTs *)
    | (TAdt (xs, n1), TAdt (ys, n2), f) :: rest
      when n1 = n2 && List.length xs = List.length ys ->
      go s (zip_with_f xs ys f @ rest)
    (* A: variable on the left *)
    | (TParam a, t, _) :: rest when not (occurs a t) ->
      let one = [(a, t)] in
      let s' = (a, t) :: List.map (fun (b, ty) -> (b, apply_ty one ty)) s in
      go s' (apply_constrs one rest)
    (* A: variable on the right *)
    | (t, TParam a, _) :: rest when not (occurs a t) ->
      let one = [(a, t)] in
      let s' = (a, t) :: List.map (fun (b, ty) -> (b, apply_ty one ty)) s in
      go s' (apply_constrs one rest)
    (* otherwise fail using the constraint's own error message *)
    | (t1, t2, f) :: _ -> Error (f t1 t2)
  in
  go [] cs


(* Rename free type vars to 'a, 'b, 'c, ... in order of first appearance,
   and return them as the scheme's quantified variables. *)
let normalize (t : ty) : ty_scheme =
  (* gather the free type vars by walking t with an accumulator *)
  let rec scan acc = function
    | TUnit | TBool | TInt | TString -> acc
    | TFun (a, b) -> scan (scan acc a) b
    | TTuple ts | TAdt (ts, _) -> List.fold_left scan acc ts
    | TParam a ->
      if List.mem a acc then acc else acc @ [a]
  in
  let order = scan [] t in
  (* map each old name to a fresh letter 'a, 'b, ... in order *)
  let rec mk_renaming i = function
    | [] -> []
    | a :: rest ->
      let name = String.make 1 (Char.chr (Char.code 'a' + i)) in
      (a, TParam name) :: mk_renaming (i + 1) rest
  in
  let renaming = mk_renaming 0 order in
  let rec mk_names i = function
    | [] -> []
    | _ :: rest ->
      String.make 1 (Char.chr (Char.code 'a' + i)) :: mk_names (i + 1) rest
  in
  (mk_names 0 order, apply_ty renaming t)


(* VAR rule: replace each quantified variable with a fresh one. what makes polymorphic uses get diff types *)
let instantiate ((vars, t) : ty_scheme) : ty =
  let s = List.map (fun a -> (a, fresh ())) vars in
  apply_ty s t


(* Type a pattern. Returns the pattern's type, the constraints
   it generates, and the new context with its variable bindings. *)
let rec infer_pat (ctxt : ctxt) (p : pattern) : ty * constr list * ctxt =
  let pos = p.pos in
  match p.pattern with
  (* WILDP: fresh type, no bindings *)
  | PWild        -> let a = fresh () in a, [], ctxt
  (* VARP: fresh type, bind var *)
  | PVar x       -> let a = fresh () in a, [], Env.add x ([], a) ctxt
  (* literal patterns: known type, no constraints, no bindings *)
  | PUnit        -> TUnit, [], ctxt
  | PBool _      -> TBool, [], ctxt
  | PInt _       -> TInt, [], ctxt
  | PString _    -> TString, [], ctxt
  (* TUPLEP: infer each sub-pattern, accumulate everything *)
  | PTuple ps ->
    let ts, cs, ctxt' =
      List.fold_left
        (fun (ts, cs, ctxt) p ->
           let t, c, ctxt' = infer_pat ctxt p in
           ts @ [t], cs @ c, ctxt')
        ([], [], ctxt) ps
    in
    TTuple ts, cs, ctxt'
  (* CONSP0: zero-arg constructor, instantiate fresh ADT params *)
  | PCons (c, None) ->
    (match Env.find_opt c ctxt with
     | Some (alphas, TAdt (params, n)) ->
       let s = List.map (fun a -> (a, fresh ())) alphas in
       TAdt (List.map (apply_ty s) params, n), [], ctxt
     | Some _ -> raise (Type_error (cons_exp_args pos c))
     | None   -> raise (Type_error (unknown_cons pos c)))
  (* CONSP1: constructor with payload, recurse on inner pattern *)
  | PCons (c, Some inner) ->
    (match Env.find_opt c ctxt with
     | Some (alphas, TFun (sigma, TAdt (params, n))) ->
       let s = List.map (fun a -> (a, fresh ())) alphas in
       let tp, cp, ctxt' = infer_pat ctxt inner in
       let cons_arg_ty = apply_ty s sigma in
       TAdt (List.map (apply_ty s) params, n),
       (cons_arg_ty, tp, exp_pat inner.pos) :: cp,
       ctxt'
     | Some _ -> raise (Type_error (cons_exp_no_args pos c))
     | None   -> raise (Type_error (unknown_cons pos c)))


(* Generate constraints for an expression. One match arm
   per typing rule from the spec. *)
let rec infer (ctxt : ctxt) (e : expr) : ty * constr list =
  let pos = e.pos in
  match e.expr with
  (* literal types *)
  | Unit     -> TUnit, []
  | Bool _   -> TBool, []
  | Int _    -> TInt, []
  | String _ -> TString, []

  (* NEGATE: operand must be int *)
  | Negate e0 ->
    let t, c = infer ctxt e0 in
    TInt, (t, TInt, exp_ty e0.pos) :: c

  (* binary operators: each has its expected operand/result types *)
  | Bop (op, e1, e2) ->
    let t1, c1 = infer ctxt e1 in
    let t2, c2 = infer ctxt e2 in
    let res, eq1, eq2 = match op with
      | Add | Sub | Mul | Div | Mod    ->
        TInt,    (t1, TInt,    exp_ty e1.pos), (t2, TInt,    exp_ty e2.pos)
      | And | Or                       ->
        TBool,   (t1, TBool,   exp_ty e1.pos), (t2, TBool,   exp_ty e2.pos)
      | Concat                         ->
        TString, (t1, TString, exp_ty e1.pos), (t2, TString, exp_ty e2.pos)
      | Eq | Neq | Lt | Lte | Gt | Gte ->
        TBool,   (t1, t2,      exp_ty e2.pos), (t1, t2,      exp_ty e2.pos)
    in
    res, eq1 :: eq2 :: c1 @ c2

  (* IF: condition is bool, both branches need to agree *)
  | If (e1, e2, e3) ->
    let t1, c1 = infer ctxt e1 in
    let t2, c2 = infer ctxt e2 in
    let t3, c3 = infer ctxt e3 in
    t3, (t1, TBool, exp_ty e1.pos) :: (t3, t2, exp_ty e3.pos) :: c1 @ c2 @ c3

  (* ANNOT: result type is the annotation, constrain inferred = annotation *)
  | Annot (e0, t') ->
    let t, c = infer ctxt e0 in
    t', (t, t', exp_ty e0.pos) :: c

  (* TUPLE: collect each component type and constraints *)
  | Tuple es ->
    let ts, cs =
      List.fold_left
        (fun (ts, cs) e ->
           let t, c = infer ctxt e in
           ts @ [t], cs @ c)
        ([], []) es
    in
    TTuple ts, cs

  (* ASSERTFALSE: assert false has any type, fresh variable *)
  | Assert {pos=_; expr=Bool false} ->
    fresh (), []
  (* ASSERT: operand must be bool, result is unit *)
  | Assert e0 ->
    let t, c = infer ctxt e0 in
    TUnit, (t, TBool, exp_ty e0.pos) :: c

  (* VAR: ins the variable's scheme with fresh vars *)
  | Var x ->
    (match Env.find_opt x ctxt with
     | Some sch -> instantiate sch, []
     | None     -> raise (Type_error (unknown_var pos x)))

  (* CONS0: zero-arg constructor *)
  | Cons (c, None) ->
    (match Env.find_opt c ctxt with
     | Some (alphas, TAdt (params, n)) ->
       let s = List.map (fun a -> (a, fresh ())) alphas in
       TAdt (List.map (apply_ty s) params, n), []
     | Some _ -> raise (Type_error (cons_exp_args pos c))
     | None   -> raise (Type_error (unknown_cons pos c)))

  (* CONS1: constructor w/ payload *)
  | Cons (c, Some e0) ->
    (match Env.find_opt c ctxt with
     | Some (alphas, TFun (sigma, TAdt (params, n))) ->
       let s = List.map (fun a -> (a, fresh ())) alphas in
       let t, c0 = infer ctxt e0 in
       TAdt (List.map (apply_ty s) params, n),
       (t, apply_ty s sigma, exp_ty e0.pos) :: c0
     | Some _ -> raise (Type_error (cons_exp_no_args pos c))
     | None   -> raise (Type_error (unknown_cons pos c)))

  (* FUN: arg has fresh type, body inferred under it *)
  | Fun ((x, None), body) ->
    let a = fresh () in
    let t, c = infer (Env.add x ([], a) ctxt) body in
    TFun (a, t), c
  (* FUNANNOT: argument type is given by the annotation *)
  | Fun ((x, Some t1), body) ->
    let t2, c = infer (Env.add x ([], t1) ctxt) body in
    TFun (t1, t2), c

  (* APP: e1 must be a function from e2's type *)
  | App (e1, e2) ->
    let t1, c1 = infer ctxt e1 in
    let t2, c2 = infer ctxt e2 in
    let a = fresh () in
    a, (t1, TFun (t2, a), fun _ _ -> invalid_app pos) :: c1 @ c2

  (* LET: infer binding, add to context, infer body *)
  | Let { is_rec = false; name; binding; body } ->
    let t1, c1 = infer ctxt binding in
    let t2, c2 = infer (Env.add name ([], t1) ctxt) body in
    t2, c1 @ c2

  (* LETREC: name gets fresh type, both binding and body see it, constrain fresh = inferred binding type *)
  | Let { is_rec = true; name; binding; body } ->
    let a = fresh () in
    let ctxt' = Env.add name ([], a) ctxt in
    let t1, c1 = infer ctxt' binding in
    let t2, c2 = infer ctxt' body in
    t2, (a, t1, exp_ty binding.pos) :: c1 @ c2

  (* MATCH: result fresh, every pattern type = e type, every arm RHS type = result type *)
  | Match (e0, arms) ->
    let t0, c0 = infer ctxt e0 in
    let tau = fresh () in
    let cs =
      List.fold_left
        (fun cs (p, rhs) ->
           let tp, cp, ctxt' = infer_pat ctxt p in
           let tr, cr = infer ctxt' rhs in
           (tp, t0, exp_pat p.pos) :: (tr, tau, exp_ty rhs.pos) :: cp @ cr @ cs)
        c0 arms
    in
    tau, cs


(* Top-level: generate constraints, unify, apply unifier, normalize.
   Type_error from infer/infer_pat carries its own message. *)
let type_of_expr (ctxt : ctxt) (e : expr) : (ty_scheme, Error_msg.t) result =
  match infer ctxt e with
  | exception Type_error err -> Error err
  | (tau, cs) ->
    (match unify cs with
     | Error err -> Error err
     | Ok s   -> Ok (normalize (apply_ty s tau)))

let rec nub l =
  match l with
  | [] -> []
  | x :: xs -> x :: List.filter ((<>) x) (nub xs)

let free_vars ty =
  let rec go = function
    | TTuple ts | TAdt (ts, _) -> List.concat_map go ts
    | TFun (t1, t2) -> go t1 @ go t2
    | TParam a -> [a]
    | _ -> []
  in nub (go ty)

let well_typed (p : stmt list) : (unit, Error_msg.t) result =
  let rec go (used_ty_names : string list) (ctxt : ctxt) p =
    match p with
    | [] -> Ok ()
    | {pos; stmt=SLet {is_rec;name;binding}} :: ps ->
      let body = Ast.Expr.var dummy_pos name in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      begin
        match type_of_expr ctxt e with
        | Ok ty -> go used_ty_names (Env.add name ty ctxt) ps
        | Error e -> Error e
      end
    | {pos; stmt=SAdt {tpars; name; constrs}} :: ps ->
      if nub tpars = tpars
      then
        if List.mem name used_ty_names
        then Error (dup_ty_name pos name)
        else
          let rec process ctxt cs =
            match cs with
            | [] -> Ok ctxt
            | (cons_name, None) :: cs ->
              let tparams = List.map (fun x -> TParam x) tpars in
              process (Env.add cons_name (tpars, TAdt(tparams, name)) ctxt) cs
            | (cons_name, Some ty) :: cs ->
              begin
                match List.(find_opt (fun x -> not (mem x tpars)) (free_vars ty)) with
                | None ->
                  let tparams = List.map (fun x -> TParam x) tpars in
                  let ctxt = Env.add cons_name (tpars, TFun (ty, TAdt(tparams, name))) ctxt in
                  process ctxt cs
                | Some a -> Error (unbound_ty_var pos a)
              end
          in
          match process ctxt constrs with
          | Ok ctxt -> go (name :: used_ty_names) ctxt ps
          | Error e-> Error e
      else Error (ty_param_several_times pos)
  in
  let ctxt =
    Env.(
      empty
      |> add "print_endline" ([], TFun (TString, TUnit))
      |> add "Nil" (["a"], TAdt ([TParam "a"], "list"))
      |> add "Cons" (["a"], TFun (TTuple [TParam "a"; TAdt ([TParam "a"], "list")], TAdt ([TParam "a"], "list")))
    )
  in go [] ctxt p

(* EVALUATION
   ----------------------------------------------------------------------
*)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VCons of string * value option
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      arg : string;
      body : expr;
    }

type dyn_env = value Env.t

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos
exception Compare_fun_val of pos

(* Checks if a value matches a pattern. Returns Some bindings if it matches, none otherwise *)
let rec match_pattern (v : value) (p : pattern)
  : (string * value) list option =
  match p.pattern, v with
  (* wildcard matches anything, no bindings *)
  | PWild, _ -> Some []
  (* variable matches anything, binds the value to the name *)
  | PVar x, _ -> Some [(x, v)]
  (* literals match if equal *)
  | PUnit, VUnit -> Some []
  | PBool b1, VBool b2 when b1 = b2 -> Some []
  | PInt n1, VInt n2 when n1 = n2 -> Some []
  | PString s1, VString s2 when s1 = s2 -> Some []
  (* tuples match elementwise, bindings concatenated *)
  | PTuple ps, VTuple vs when List.length ps = List.length vs ->
    let rec walk ps vs acc =
      match ps, vs with
      | [], [] -> Some acc
      | p :: ps', v :: vs' ->
        (match match_pattern v p with
         | None    -> None
         | Some bs -> walk ps' vs' (acc @ bs))
      | _ -> None
    in
    walk ps vs []
  (* constructor with no payload *)
  | PCons (c1, None), VCons (c2, None) when c1 = c2 -> Some []
  (* constructor with payload, recurse on the inner pattern *)
  | PCons (c1, Some p), VCons (c2, Some v) when c1 = c2 ->
    match_pattern v p
  (* fails if some thing else *)
  | _ -> None

(* Compares two values for =, <>, <, <=, >, >=. Raises Compare_fun_val if either side is a closure *)
let rec compare_vals pos (v1 : value) (v2 : value) : int =
  match v1, v2 with
  (* closures cannot be compared *)
  | VClos _, _ | _, VClos _ -> raise (Compare_fun_val pos)
  | VUnit, VUnit -> 0
  | VBool a, VBool b -> compare a b
  | VInt a, VInt b -> compare a b
  | VString a, VString b -> compare a b
  (* tuples: lexicographic *)
  | VTuple xs, VTuple ys -> compare_lists pos xs ys
  (* constructors: compare names, then payloads *)
  | VCons (c1, None), VCons (c2, None) -> compare c1 c2
  | VCons (c1, Some a), VCons (c2, Some b) ->
    let c = compare c1 c2 in
    if c <> 0 then c else compare_vals pos a b
  | VCons (_, None), VCons (_, Some _) -> -1
  | VCons (_, Some _), VCons (_, None) ->  1
  | _ -> compare v1 v2

(* Lexicographic compare on two value lists *)
and compare_lists pos l1 l2 : int =
  match l1, l2 with
  | [], []     -> 0
  | [], _      -> -1
  | _,  []     ->  1
  | x :: xs, y :: ys ->
    let c = compare_vals pos x y in
    if c <> 0 then c else compare_lists pos xs ys


(* Big-step evaluator. One match arm per evaluation rule from the spec. *)
let rec eval_expr (env : dyn_env) (e : Ast.Expr.t) : value =
  let pos = e.pos in
  match e.expr with
  (* literals evaluate to themselves *)
  | Unit     -> VUnit
  | Bool b   -> VBool b
  | Int n    -> VInt n
  | String s -> VString s

  (* unary minus on an int *)
  | Negate e0 ->
    (match eval_expr env e0 with
     | VInt n -> VInt (-n)
     | _      -> assert false)

  (* short-circuit && *)
  | Bop (And, e1, e2) ->
    (match eval_expr env e1 with
     | VBool false -> VBool false
     | VBool true  -> eval_expr env e2
     | _ -> assert false)
  (* short-circuit || *)
  | Bop (Or, e1, e2) ->
    (match eval_expr env e1 with
     | VBool true  -> VBool true
     | VBool false -> eval_expr env e2
     | _ -> assert false)

  (* other binary ops: evaluate both sides, then dispatch *)
  | Bop (op, e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match op, v1, v2 with
     | Add, VInt a, VInt b -> VInt (a + b)
     | Sub, VInt a, VInt b -> VInt (a - b)
     | Mul, VInt a, VInt b -> VInt (a * b)
     (* division by zero raises Div_by_zero *)
     | Div, VInt _, VInt 0 -> raise (Div_by_zero pos)
     | Div, VInt a, VInt b -> VInt (a / b)
     | Mod, VInt _, VInt 0 -> raise (Div_by_zero pos)
     | Mod, VInt a, VInt b -> VInt (a mod b)
     (* string concat *)
     | Concat, VString a, VString b -> VString (a ^ b)
     (* comparisons all go through compare_vals *)
     | Eq,  _, _ -> VBool (compare_vals pos v1 v2 =  0)
     | Neq, _, _ -> VBool (compare_vals pos v1 v2 <> 0)
     | Lt,  _, _ -> VBool (compare_vals pos v1 v2 <  0)
     | Lte, _, _ -> VBool (compare_vals pos v1 v2 <= 0)
     | Gt,  _, _ -> VBool (compare_vals pos v1 v2 >  0)
     | Gte, _, _ -> VBool (compare_vals pos v1 v2 >= 0)
     | (And | Or), _, _ -> assert false
     | _ -> assert false)

  (* if-then-else: only evaluate the chosen branch *)
  | If (e1, e2, e3) ->
    (match eval_expr env e1 with
     | VBool true  -> eval_expr env e2
     | VBool false -> eval_expr env e3
     | _ -> assert false)

  (* type annotations erased at runtime *)
  | Annot (e0, _) -> eval_expr env e0

  (* tuple: evaluate each component left-to-right *)
  | Tuple es -> VTuple (List.map (eval_expr env) es)

  (* assert e, unit if true, Assert_fail if false *)
  | Assert e0 ->
    (match eval_expr env e0 with
     | VBool true  -> VUnit
     | VBool false -> raise (Assert_fail pos)
     | _ -> assert false)

  (* variable lookup *)
  | Var x -> Env.find x env

  (* constructor values *)
  | Cons (c, None)    -> VCons (c, None)
  | Cons (c, Some e0) -> VCons (c, Some (eval_expr env e0))

  (* function lit becomes a closure capturing the current env *)
  | Fun ((x, _), body) ->
    VClos { env; name = None; arg = x; body }

  (* function application *)
  | App (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
     (* print_endline is built-in, do the side-effect, return unit *)
     | VClos { name = None; arg = "$print_endline"; _ } ->
       (match v2 with
        | VString s -> print_endline s; VUnit
        | _ -> assert false)
     (* unnamed closure, bind arg in the captured env, run body *)
     | VClos { env = cenv; name = None; arg; body } ->
       eval_expr (Env.add arg v2 cenv) body
     (* named closure: also bind the name to itself for rec *)
     | VClos { env = cenv; name = Some f; arg; body } ->
       let cenv' = Env.add f v1 cenv in
       eval_expr (Env.add arg v2 cenv') body
     | _ -> assert false)

  (* let x = e1 in e2 *)
  | Let { is_rec = false; name; binding; body } ->
    let v = eval_expr env binding in
    eval_expr (Env.add name v env) body

  (* let rec f = fun x -> ... in e2, make a named closure *)
  | Let { is_rec = true; name; binding; body } ->
    (match binding.expr with
     | Fun ((x, _), fbody) ->
       let clos = VClos { env; name = Some name; arg = x; body = fbody } in
       eval_expr (Env.add name clos env) body
     | _ -> assert false)

  (* match: try each arm, first matching pattern wins *)
  | Match (e0, arms) ->
    let v0 = eval_expr env e0 in
    let rec try_arms = function
      | [] -> raise (Match_fail pos)
      | (p, body) :: rest ->
        (match match_pattern v0 p with
         | None    -> try_arms rest
         | Some bs ->
           let env' =
             List.fold_left (fun acc (x, v) -> Env.add x v acc) env bs
           in
           eval_expr env' body)
    in
    try_arms arms

let eval (p : stmt list) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
    | _ :: ps -> go env v ps
  in
  let env =
    Env.(
      empty
      |> add "print_endline"
        (VClos
           {
             env = empty;
             name = None;
             arg = "$print_endline";
             body = Ast.Expr.mk dummy_pos Unit;
           })
    )
  in go env None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* () = well_typed prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
    | exception Compare_fun_val pos -> Error (Error_msg.mk pos "(Exception) Compare_fun_val")
  in
  Ok v

(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _x = " ^ s in
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false
