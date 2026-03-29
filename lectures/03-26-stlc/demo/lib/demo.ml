
type ty = Ast.ty =
  | Int
  | TFun of ty * ty

type expr = Ast.expr =
  | Var of string
  | Num of int
  | Fun of string * ty * expr
  | App of expr * expr
  | Let of string * expr * expr

module Env = Map.Make(String)
(* CHEATSHEET:
   ∅ ≡ Env.empty
   ℰ[x] ≡ Env.find x ℰ    or    Env.find_opt x ℰ
   ℰ[x ↦ v] ≡ Env.add x v ℰ
*)

type value =
  | VNum of int
  | VBool of bool
  | VClos of value Env.t * expr

type dyn_env = value Env.t

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

type ctxt = ty Env.t

let rec type_of (ctxt : ctxt) (e : expr) : ty option =
  match e with
    (*
       (x : τ) ∈ Γ
       ─────────── (var)
       Γ ⊢ x : τ
    *)
  | Var x -> Env.find_opt x ctxt
    (*
       n is an int lit
       ─────────────── (intLit)
         Γ ⊢ n : int
    *)
  | Num _ -> Some Int
    (*
              Γ, x : τ₁ ⊢ e : τ₂
       ───────────────────────────────── (fun)
         Γ ⊢ fun (x : τ₁) → e : τ₁ → τ₂
    *)
  | Fun (x, in_ty, e) ->
    begin
      let ctxt = Env.add x in_ty ctxt in
      match type_of ctxt e with
      | Some out_ty -> Some (TFun (in_ty, out_ty))
      | None -> None
    end
    (*
       Γ ⊢ e₁ : τ₂ → τ      Γ ⊢ e₂ : τ₂
       ───────────────────────────────── (app)
                 Γ ⊢ e₁ e₂ : τ
    *)
  | App (e1, e2) ->
    begin
      match type_of ctxt e1, type_of ctxt e2 with
      | Some (TFun (in_ty, out_ty)), Some t2 ->
        if in_ty = t2
        then Some out_ty
        else None
      | _ -> None
    end
    (*
       Γ ⊢ e₁ : τ₁       Γ, τ₁ ⊢ e₂ : τ₂
       ───────────────────────────────── (let)
           Γ ⊢ let x = e₁ in e₂ : τ
    *)

  | Let (x, e1, e2) ->
    begin
      match type_of ctxt e1 with
      | Some t1 ->
        let ctxt = Env.add x t1 ctxt in
        type_of ctxt e2
      | None -> None
    end

let rec eval (env : dyn_env) (e : expr) : value =
  match e with
  | Var x ->
    (*
        ℰ[x] = v
       ────────── (varE)
       ℰ ⊢ x ⇓ v
    *)
    Env.find x env
  | Num n ->
    (*
       n is an int lit
       ─────────────── (intLitE)
         ℰ ⊢ n ⇓ n
    *)
    VNum n
  | Fun (x, ty, e) ->
    (*
       ────────────────────────────────────── (funE)
       ℰ ⊢ fun x → e ⇓ ⦇ ℰ , fun x → e ⦈
    *)
    VClos (env, Fun (x, ty, e))
  | App (e1, e2) ->
    (*
       ℰ₁ ⊢ e₁ ⇓ ⦇ ℰ' , λ x . e ⦈      ℰ₁ ⊢ e₂ ⇓ v₂      ℰ₂ = ℰ'[x ↦ v₂]      ℰ₂ ⊢ e ⇓ v
       ───────────────────────────────────────────────────────────────────────────────────── (appE)
                                          ℰ ⊢ e₁ e₂ ⇓ v
    *)
    begin
      match eval env e1 with
      | VClos (env', Fun (x, _, e)) ->
        let v2 = eval env e2 in
        let env2 = Env.add x v2 env' in
        let v = eval env2 e in
        v
      | _ -> assert false
    end
  | Let (x, e1, e2) ->
    (*
       ℰ ⊢ e₁ ⇓ v₁      ℰ' = ℰ[x ↦ v₁]      ℰ' ⊢ e₂ ⇓ v
       ─────────────────────────────────────────────────── (letE)
                    ℰ ⊢ let x = e₁ in e₂ ⇓ v
    *)
    let v1 = eval env e1 in
    let env' = Env.add x v1 env in
    let v = eval env' e2 in
    v

let interp (s : string) : value option =
  match parse s with
  | Some expr ->
    begin
      match type_of Env.empty expr with
      | Some _ ->
        begin
          match eval Env.empty expr with
          | v -> Some v
          | exception _ -> print_endline "Eval error"; None
        end
      | None -> print_endline "Type error"; None
    end
  | None -> print_endline "Parse error"; None
