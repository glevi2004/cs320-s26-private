
(** Returns true if the character is ASCII whitespace. *)
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

type token = Lpar | Rpar | Word of string

(** Tokenizes a string into a list of S-expression tokens (left/right
    parens and words). Scans let-to-right, acumulating in reverse,
    then reverses at the end. *)
let tokens_of_string (s : string) : token list =
  let rec go acc i =
    if i >= String.length s
    then acc
    else
      match s.[i] with
      | '(' -> go (Lpar :: acc) (i + 1)
      | ')' -> go (Rpar :: acc) (i + 1)
      | c ->
        if is_ws c
        then go acc (i + 1)
        else
          let rec go' j =
            if i + j >= String.length s
            then Word (String.sub s i j) :: acc
            else
              let c = s.[i + j] in
              if List.mem c ['('; ')'] || is_ws c
              then go (Word (String.sub s i j) :: acc) (i + j)
              else go' (j + 1)
          in go' 1
  in List.rev (go [] 0)

type sexpr =
  | Atom of string
  | List of sexpr list

(** Parses a token list into a single S-expression, returning [None]
    if the tokens do not form exactly one well-formed S-expression. *)
let sexpr_of_tokens_opt (ts : token list) : sexpr option =
  let rec go (ts : token list) : (sexpr * token list) option =
    match ts with
    | [] -> None
    | t :: ts -> (
        match t with
        | Word a -> Some (Atom a, ts)
        | Lpar -> (
            match go' ts with
            | es, Rpar :: ts -> Some (List es, ts)
            | _ -> None
          )
        | Rpar -> None
      )
  and go' (ts : token list) : sexpr list * token list =
    match go ts with
    | Some (e, ts) ->
      let (es, ts) = go' ts in
      e :: es, ts
    | None -> [], ts
  in
  match go ts with
  | Some (e, []) -> Some e
  | _ -> None

(** Parses a string into an S-expression, or [None] if invalid. *)
let sexpr_of_string_opt (s : string) : sexpr option =
  sexpr_of_tokens_opt (tokens_of_string s)

(** Converts an S-expression back into its string representation. *)
let rec string_of_sexpr (e : sexpr) : string =
  match e with
  | Atom s -> s
  | List ss -> "(" ^ String.concat " " (List.map string_of_sexpr ss) ^ ")"

type op = Add | Mul | Eq

(** Returns the string symbol for an operator: [+], [*], or [=]. *)
let string_of_op (op : op) : string =
  match op with
  | Add -> "+"
  | Mul -> "*"
  | Eq -> "="

(** Parses an S-expression atom into an [op], or [None] if it is not
    one of ["+"], ["*"], ["="]. *)
let op_of_sexpr_opt (s : sexpr) : op option =
  match s with
  | Atom "+" -> Some Add
  | Atom "*" -> Some Mul
  | Atom "=" -> Some Eq
  | _ -> None

type expr =
  | Int of int
  | Bop of op * expr * expr
  | If of expr * expr * expr

(** Converts an S-expresion into an arithmetic [expr].
    - [Atom a] is parsed as [Int n] if [a] is a valid integer string.
    - [List \[op; e1; e2\]] is parsed as [Bop] if [op] is a valid operator.
    - [List \[Atom "if"; e1; e2; e3\]] is parsed as [If].
    - Returns [None] for anything else. *)
let rec expr_of_sexpr_opt (s : sexpr) : expr option =
  match s with
  | Atom a -> (
      match int_of_string_opt a with
      | Some n -> Some (Int n)
      | None -> None
    )
  | List [op_s; e1_s; e2_s] -> (
      match op_of_sexpr_opt op_s with
      | Some op -> (
          match expr_of_sexpr_opt e1_s, expr_of_sexpr_opt e2_s with
          | Some e1, Some e2 -> Some (Bop (op, e1, e2))
          | _ -> None
        )
      | None -> None
    )
  | List [Atom "if"; e1_s; e2_s; e3_s] -> (
      match expr_of_sexpr_opt e1_s, expr_of_sexpr_opt e2_s, expr_of_sexpr_opt e3_s with
      | Some e1, Some e2, Some e3 -> Some (If (e1, e2, e3))
      | _ -> None
    )
  | _ -> None

(** Parses a string into an [expr] by composing [sexpr_of_string_opt]
    and [expr_of_sexpr_opt]. Returns [None] on any parse failure. *)
let expr_of_string_opt (s : string) : expr option =
  match sexpr_of_string_opt s with
  | Some se -> expr_of_sexpr_opt se
  | None -> None

(** Converts an [expr] into its S-expresion representation. This is
    total: every [expr] has a valid S-expresion form. *)
let rec sexpr_of_expr (e : expr) : sexpr =
  match e with
  | Int n -> Atom (string_of_int n)
  | Bop (op, e1, e2) ->
    List [Atom (string_of_op op); sexpr_of_expr e1; sexpr_of_expr e2]
  | If (e1, e2, e3) ->
    List [Atom "if"; sexpr_of_expr e1; sexpr_of_expr e2; sexpr_of_expr e3]

(** Converts an [expr] to its string representation by composing
    [sexpr_of_expr] and [string_of_sexpr]. *)
let string_of_expr (e : expr) : string =
  string_of_sexpr (sexpr_of_expr e)

type ty = BoolT | IntT

(** Parses an S-expression atom into a [ty]: ["int"] -> [IntT],
    ["bool"] -> [BoolT], anything else -> [None]. *)
let ty_of_sexpr_opt (e : sexpr) : ty option =
  match e with
  | Atom s -> (
      match s with
      | "bool" -> Some BoolT
      | "int" -> Some IntT
      | _ -> None
    )
  | _ -> None

(** Returns the string name of a type: ["int"] or ["bool"]. *)
let string_of_ty (ty : ty) : string =
  match ty with
  | BoolT -> "bool"
  | IntT -> "int"

type ty_jmt =
  {
    expr : expr;
    ty : ty;
  }

(** Pretty prints a typing judgment as ["<expr> : <ty>"] *)
let string_of_ty_jmt (j : ty_jmt) : string =
  string_of_expr j.expr ^ " : " ^ string_of_ty j.ty

type ty_rule =
  | Int_lit
  | Add_int
  | Mul_int
  | Eq_rule
  | If_rule

(** Returns the display name of a typing rule. *)
let string_of_ty_rule (r : ty_rule) =
  match r with
  | Int_lit -> "intLit"
  | Add_int -> "addInt"
  | Mul_int -> "mulInt"
  | Eq_rule -> "eq"
  | If_rule -> "if"

(** Parses an S-expression atom into a [ty_rule] by matching against
    the rule name strings: INTLIT, ADDINT, MULINT, EQ, IF. *)
let ty_rule_of_sexpr_opt (e : sexpr) : ty_rule option =
  match e with
  | Atom s -> (
      match s with
      | "INTLIT" -> Some Int_lit
      | "ADDINT" -> Some Add_int
      | "MULINT" -> Some Mul_int
      | "EQ" -> Some Eq_rule
      | "IF" -> Some If_rule
      | _ -> None
    )
  | _ -> None

type ty_deriv =
  | Rule_app of {
      prem_derivs : ty_deriv list;
      concl : ty_jmt;
      rname : ty_rule;
    }
  | Hole

(** Parses an S-expression into a [ty_deriv].
    - [Atom "???"] becomes [Hole].
    - [List (expr :: ty :: rname :: derivs...)] becomes a [Rule_app]
      which its premise derivations are parsed recursively.
    - Returns [None] if the S-expression is malformed. *)
let rec ty_deriv_of_sexpr_opt (s : sexpr) : ty_deriv option =
  match s with
  | Atom "???" -> Some Hole
  | List (expr_s :: ty_s :: rname_s :: deriv_ss) -> (
      match expr_of_sexpr_opt expr_s, ty_of_sexpr_opt ty_s, ty_rule_of_sexpr_opt rname_s with
      | Some expr, Some ty, Some rname ->
        let rec go acc = function
          | [] -> Some (List.rev acc)
          | d :: ds -> (
              match ty_deriv_of_sexpr_opt d with
              | Some deriv -> go (deriv :: acc) ds
              | None -> None
            )
        in (
          match go [] deriv_ss with
          | Some prem_derivs ->
            Some (Rule_app { prem_derivs; concl = { expr; ty }; rname })
          | None -> None
        )
      | _ -> None
    )
  | _ -> None

(** Parses a string into a [ty_deriv] by composing [sexpr_of_string_opt]
    and [ty_deriv_of_sexpr_opt]. Returns [None] on any parse failure. *)
let ty_deriv_of_string_opt (s : string) : ty_deriv option =
  match sexpr_of_string_opt s with
  | Some se -> ty_deriv_of_sexpr_opt se
  | None -> None

(** Pretty-prints a typing derivation as a tree with Unicode box-drawing
    characters, with rule names right-aligned on each line. *)
let string_of_ty_deriv (d : ty_deriv) : string =
  let rec go d =
    match d with
    | Hole -> [("???", "hole")]
    | Rule_app d ->
      (string_of_ty_jmt d.concl, string_of_ty_rule d.rname) :: go' [] d.prem_derivs
  and go' has_line ds =
    let lines =
      List.fold_left
        (fun acc b -> (if b then "│  " else "   ") ^ acc)
        ""
        has_line
    in
    match ds with
    | [] -> []
    | [Hole] ->
      [lines ^ "└──???" , "hole"]
    | [Rule_app d] ->
      let next_line =
        ( lines ^ "└──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in next_line :: go' (false :: has_line) d.prem_derivs
    | Hole :: ds -> (lines ^ "├──???" , "hole") :: go' has_line ds
    | Rule_app d :: ds ->
      let next_line =
        ( lines ^ "├──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in
      next_line
      :: go' (true :: has_line) d.prem_derivs
      @ go' has_line ds
  in
  let lines = go d in
  let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0 in
  let width =
    List.fold_left
      (fun acc (line, _) -> max acc (length line))
      0
      lines
  in
  let lines =
    List.map
      (fun (line, rname) ->
         String.concat ""
           [
             line;
             String.init (width - length line + 2) (fun _ -> ' ');
             "("; rname; ")";
           ])
      lines
  in
  String.concat "\n" lines

(** Checks that a single rule application is consistent: [check_rule rname
    prems concl] returns [true] if [concl] follows from [prems] under the
    typing rule [rname]. Premises that are [None] (holes) are treated as
    automatically satisfying any constraint *)
let check_rule (rname : ty_rule) (prems : ty_jmt option list) (concl : ty_jmt) : bool =
  match rname with
  | Int_lit -> (
      match concl.expr, concl.ty, prems with
      | Int _, IntT, [] -> true
      | _ -> false
    )
  | Add_int -> (
      match concl.expr, concl.ty, prems with
      | Bop (Add, e1, e2), IntT, [p1; p2] ->
        let ok p e =
          match p with
          | None -> true
          | Some j -> j.expr = e && j.ty = IntT
        in
        ok p1 e1 && ok p2 e2
      | _ -> false
    )
  | Mul_int -> (
      match concl.expr, concl.ty, prems with
      | Bop (Mul, e1, e2), IntT, [p1; p2] ->
        let ok p e =
          match p with
          | None -> true
          | Some j -> j.expr = e && j.ty = IntT
        in
        ok p1 e1 && ok p2 e2
      | _ -> false
    )
  | Eq_rule -> (
      match concl.expr, concl.ty, prems with
      | Bop (Eq, e1, e2), BoolT, [p1; p2] ->
        let ok p e =
          match p with
          | None -> true
          | Some j -> j.expr = e
        in
        let types_ok =
          match p1, p2 with
          | Some j1, Some j2 -> j1.ty = j2.ty
          | _ -> true
        in
        ok p1 e1 && ok p2 e2 && types_ok
      | _ -> false
    )
  | If_rule -> (
      match concl.expr, prems with
      | If (e1, e2, e3), [p1; p2; p3] ->
        let ok p e ty =
          match p with
          | None -> true
          | Some j -> j.expr = e && j.ty = ty
        in
        ok p1 e1 BoolT &&
        ok p2 e2 concl.ty &&
        ok p3 e3 concl.ty
      | _ -> false
    )

type status =
  | Complete
  | Invalid
  | Partial

(** Recursively checks an entire derivation tree. Returns [Complete] if
    every rule application is valid and there are no holes, [Partial] if
    all present rule applications are valid but holes remain, or [Invalid]
    if any rule application is inconsistent. *)
let rec check_deriv (d : ty_deriv) : status =
  match d with
  | Hole -> Partial
  | Rule_app { prem_derivs; concl; rname } ->
    let prem_jmts =
      List.map
        (fun d ->
           match d with
           | Hole -> None
           | Rule_app r -> Some r.concl)
        prem_derivs
    in
    if not (check_rule rname prem_jmts concl)
    then Invalid
    else
      let statuses = List.map check_deriv prem_derivs in
      if List.mem Invalid statuses then Invalid
      else if List.mem Partial statuses then Partial
      else Complete

type value = BoolV of bool | IntV of int

(** Converts a [value] to its string representation. *)
let string_of_value (v : value) : string =
  match v with
  | BoolV b -> string_of_bool b
  | IntV n -> string_of_int n

(** Evaluates an [expr] to a [value] according to the language semantics.
    Add and multiplication operate on ints; equality compares
    any two values; if-expressions evaluate only the taken
    branch. Behavior is undefined for ill-typed expressions. *)
let rec value_of_expr (e : expr) : value =
  match e with
  | Int n -> IntV n
  | Bop (Add, e1, e2) -> (
      match value_of_expr e1, value_of_expr e2 with
      | IntV v1, IntV v2 -> IntV (v1 + v2)
      | _ -> assert false
    )
  | Bop (Mul, e1, e2) -> (
      match value_of_expr e1, value_of_expr e2 with
      | IntV v1, IntV v2 -> IntV (v1 * v2)
      | _ -> assert false
    )
  | Bop (Eq, e1, e2) ->
    BoolV (value_of_expr e1 = value_of_expr e2)
  | If (e1, e2, e3) -> (
      match value_of_expr e1 with
      | BoolV true -> value_of_expr e2
      | BoolV false -> value_of_expr e3
      | _ -> assert false
    )

type error = Parse_error | Invalid_deriv of ty_deriv

let interp (s : string) : (ty_deriv * value option, error) result  =
  match ty_deriv_of_string_opt s with
  | Some deriv -> (
    match check_deriv deriv with
    | Complete -> (
      match deriv with
      | Rule_app d -> Ok (deriv, Some (value_of_expr d.concl.expr))
      | _ -> assert false
    )
    | Partial -> Ok (deriv, None)
    | Invalid -> Error (Invalid_deriv deriv)
  )
  | None -> Error Parse_error

let example_deriv : string =
  "((if (= (= 5 (+ 1 4)) (= 0 1)) (+ 2 3) (* (+ 4 5) 67)) int IF \
   ((= (= 5 (+ 1 4)) (= 0 1)) bool EQ \
   ((= 5 (+ 1 4)) bool EQ \
   (5 int INTLIT) \
   ((+ 1 4) int ADDINT \
   (1 int INTLIT) \
   (4 int INTLIT))) \
   ((= 0 1) bool EQ \
   (0 int INTLIT) \
   (1 int INTLIT))) \
   ((+ 2 3) int ADDINT \
   (2 int INTLIT) \
   (3 int INTLIT)) \
   ((* (+ 4 5) 67) int MULINT \
   ((+ 4 5) int ADDINT \
   (4 int INTLIT) \
   (5 int INTLIT)) \
   (67 int INTLIT)))"
