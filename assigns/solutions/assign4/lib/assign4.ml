
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

type token = Lpar | Rpar | Word of string

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

let sexpr_of_string_opt (s : string) : sexpr option =
  sexpr_of_tokens_opt (tokens_of_string s)

let rec string_of_sexpr (e : sexpr) : string =
  match e with
  | Atom s -> s
  | List ss -> "(" ^ String.concat " " (List.map string_of_sexpr ss) ^ ")"

type op = Add | Mul | Eq

let string_of_op (op : op) : string =
  match op with
  | Add -> "+"
  | Mul -> "*"
  | Eq -> "="

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

let expr_of_sexpr_opt (e : sexpr) : expr option =
  let rec go e =
    match e with
    | List [Atom "if"; e1; e2; e3] -> (
        match go e1, go e2, go e3 with
        | Some e1, Some e2, Some e3 -> Some (If (e1, e2, e3))
        | _ -> None
      )
    | List [op; e1; e2] -> (
        match op_of_sexpr_opt op, go e1, go e2 with
        | Some op, Some e1, Some e2 ->
          Some (Bop (op, e1, e2))
        | _ -> None
      )
    | Atom n -> (
        match int_of_string_opt n with
        | Some n -> Some (Int n)
        | None -> None
      )
    | _ -> None
  in go e

let expr_of_string_opt (s : string) : expr option =
  match sexpr_of_string_opt s with
  | Some e -> expr_of_sexpr_opt e
  | None -> None

let rec sexpr_of_expr (e : expr) : sexpr =
  match e with
  | Int n -> Atom (string_of_int n)
  | Bop (op, e1, e2) ->
    List
      [
        Atom (string_of_op op);
        sexpr_of_expr e1;
        sexpr_of_expr e2;
      ]
  | If (e1, e2, e3) ->
    List
      [
        Atom "if";
        sexpr_of_expr e1;
        sexpr_of_expr e2;
        sexpr_of_expr e3;
      ]

let string_of_expr (e : expr) : string =
  string_of_sexpr (sexpr_of_expr e)

type ty = BoolT | IntT

let ty_of_sexpr_opt (e : sexpr) : ty option =
  match e with
  | Atom s -> (
      match s with
      | "bool" -> Some BoolT
      | "int" -> Some IntT
      | _ -> None
    )
  | _ -> None

let string_of_ty (ty : ty) : string =
  match ty with
  | BoolT -> "bool"
  | IntT -> "int"

type ty_jmt =
  {
    expr : expr;
    ty : ty;
  }

let string_of_ty_jmt (j : ty_jmt) : string =
  string_of_expr j.expr ^ " : " ^ string_of_ty j.ty

type ty_rule =
  | Int_lit
  | Add_int
  | Mul_int
  | Eq_rule
  | If_rule

let string_of_ty_rule (r : ty_rule) =
  match r with
  | Int_lit -> "intLit"
  | Add_int -> "addInt"
  | Mul_int -> "mulInt"
  | Eq_rule -> "eq"
  | If_rule -> "if"

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

let ty_deriv_of_sexpr_opt (e : sexpr) : ty_deriv option =
  let rec go = function
    | List (e :: ty :: rname :: prem_derivs) -> (
        match expr_of_sexpr_opt e, ty_of_sexpr_opt ty, ty_rule_of_sexpr_opt rname, go' prem_derivs with
        | Some e, Some ty, Some rname, Some prem_derivs ->
          Some (Rule_app {concl = {expr = e; ty = ty}; rname; prem_derivs })
        | _ -> None
      )
    | Atom "???" -> Some Hole
    | _ -> None
  and go' = function
    | [] -> Some []
    | d :: ds -> (
        match go d, go' ds with
        | Some d, Some ds -> Some (d :: ds)
        | _ -> None
      )
  in go e

let ty_deriv_of_string_opt (s : string) : ty_deriv option =
  match sexpr_of_string_opt s with
  | Some e -> ty_deriv_of_sexpr_opt e
  | _ -> None

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

let check_int_lit_rule (ps : ty_jmt option list) (c : ty_jmt) : bool =
  match ps, c.expr, c.ty with
  | [], Int _, IntT -> true
  | _ -> false

let check_op_rule op ps c =
  match ps, c.expr, c.ty with
  | [j1; j2], Bop (o, e1, e2), IntT ->
    let left =
      match j1 with
      | Some j1 -> j1.expr = e1 && j1.ty = IntT
      | None -> true
    in
    let right =
      match j2 with
      | Some j2 -> j2.expr = e2 && j2.ty = IntT
      | None -> true
    in
    o = op && left && right
  | _ -> false

let check_add_rule (ps : ty_jmt option list) (c : ty_jmt) : bool =
  check_op_rule Add ps c

let check_mul_rule (ps : ty_jmt option list) (c : ty_jmt) : bool =
  check_op_rule Mul ps c

let check_eq_rule (ps : ty_jmt option list) (c : ty_jmt) : bool =
  match ps, c.expr, c.ty with
  | [j1; j2], Bop (Eq, e1, e2), BoolT -> (
      match j1, j2 with
      | Some j1, Some j2 ->
        j1.expr = e1
        && j2.expr = e2
        && j1.ty = j2.ty
      | Some j1, None -> j1.expr = e1
      | None, Some j2 -> j2.expr = e2
      | None, None -> true
    )
  | _ -> false

let check_if_rule (ps : ty_jmt option list) (c : ty_jmt) : bool =
  match ps, c.expr, c.ty with
  | [j1; j2; j3], If (e1, e2, e3), ty ->
    let cond =
      match j1 with
      | Some j1 -> j1.expr = e1 && j1.ty = BoolT
      | None -> true
    in
    let branches =
      match j2, j3 with
      | Some j2, Some j3 ->
        j2.expr = e2
        && j3.expr = e3
        && j2.ty = j3.ty
        && j2.ty = ty
      | Some j2, None -> j2.expr = e2 && j2.ty = ty
      | None, Some j3 -> j3.expr = e3 && j3.ty = ty
      | None, None -> true
    in
    cond && branches
  | _ -> false

let check_rule (rname : ty_rule) (ps : ty_jmt option list) (c : ty_jmt) : bool =
  match rname with
  | Int_lit -> check_int_lit_rule ps c
  | Add_int -> check_add_rule ps c
  | Mul_int -> check_mul_rule ps c
  | Eq_rule -> check_eq_rule ps c
  | If_rule -> check_if_rule ps c

type status =
  | Complete
  | Invalid
  | Partial

let status_of_derivs =
  let rec loop acc l =
    match l with
    | [] -> acc
    | Complete :: l -> loop acc l
    | Partial :: l -> loop Partial l
    | Invalid :: _ -> Invalid
  in loop Complete

let prems ds =
  let rec loop acc ds =
    match ds with
    | [] -> List.rev acc
    | Hole :: ds -> loop (None :: acc) ds
    | Rule_app d :: ds -> loop (Some d.concl :: acc) ds
  in loop [] ds

let rec check_deriv (d : ty_deriv) : status =
  match d with
  | Hole -> Partial
  | Rule_app d ->
    if check_rule d.rname (prems d.prem_derivs) d.concl
    then status_of_derivs (List.map check_deriv d.prem_derivs)
    else Invalid

type value = BoolV of bool | IntV of int

let string_of_value (v : value) : string =
  match v with
  | BoolV b -> string_of_bool b
  | IntV n -> string_of_int n

let rec value_of_expr (e : expr) : value =
  match e with
  | Int n -> IntV n
  | Bop (Add, e1, e2) -> (
      match value_of_expr e1, value_of_expr e2 with
      | IntV m, IntV n -> IntV (m + n)
      | _ -> assert false
    )
  | Bop (Mul, e1, e2) -> (
      match value_of_expr e1, value_of_expr e2 with
      | IntV m, IntV n -> IntV (m * n)
      | _ -> assert false
    )
  | Bop (Eq, e1, e2) -> BoolV (value_of_expr e1 = value_of_expr e2)
  | If (e1, e2, e3) -> (
      match value_of_expr e1 with
      | BoolV b ->
        if b
        then value_of_expr e2
        else value_of_expr e3
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

let example_deriv : string = "
((if (= (= 5 (+ 1 4)) (= 0 1)) (+ 2 3) (* (+ 4 5) 67)) int IF
    ((= (= 5 (+ 1 4)) (= 0 1)) bool EQ
        ((= 5 (+ 1 4)) bool EQ
            (5 int INTLIT)
            ((+ 1 4) int ADDINT
                (1 int INTLIT)
                (4 int INTLIT)))
        ((= 0 1) bool EQ
            (0 int INTLIT)
            (1 int INTLIT)))
    ((+ 2 3) int ADDINT
        (2 int INTLIT)
        (3 int INTLIT))
    ((* (+ 4 5) 67) int MULINT
        ((+ 4 5) int ADDINT
            (4 int INTLIT)
            (5 int INTLIT))
        (67 int INTLIT)))"
