module Tensor = Tensor
type tensor = Tensor.t

type 'a sexpr = 'a Sexpr.t =
  | Atom of 'a
  | List of 'a sexpr list

type op = Syntax.op = Add | Mul

type expr = Syntax.expr =
  | Ident of string * string list
  | Map of op * expr * expr
  | Fold of op * string * expr

type stmt = Syntax.stmt =
  | Init of string * int list * float sexpr
  | Set of string * string list * expr

let dim_check (env : (string * tensor) list) (e : expr) : ((string * int) list) option =
  let distinct (labels : string list) : bool =
    let rec go (seen : string list) (l : string list) : bool =
      match l with
      | [] -> true
      | x :: xs ->
        if List.mem x seen then false
        else go (x :: seen) xs
    in go [] labels
  in
  let consistent (idx1 : (string * int) list) (idx2 : (string * int) list) : bool =
    List.for_all (fun (label, size) ->
      match List.assoc_opt label idx2 with
      | None -> true
      | Some size2 -> size = size2
    ) idx1
  in
  let union (idx1 : (string * int) list) (idx2 : (string * int) list) : (string * int) list =
    let extra = List.filter (fun (label, _) -> not (List.mem_assoc label idx1)) idx2 in
    idx1 @ extra
  in
  let rec check (e : expr) : (string * int) list option =
    match e with
    | Ident (a, labels) -> (
      match List.assoc_opt a env with
      | None -> None
      | Some t ->
        let idx = Tensor.idx_space t in
        if List.length labels <> List.length idx then None
        else if not (distinct labels) then None
        else Some (List.map2 (fun label (_, size) -> (label, size)) labels idx)
    )
    | Map (_, e1, e2) -> (
      match check e1, check e2 with
      | Some idx1, Some idx2 ->
        if consistent idx1 idx2 then Some (union idx1 idx2)
        else None
      | _ -> None
    )
    | Fold (_, i, e) -> (
      match check e with
      | Some idx ->
        if List.mem_assoc i idx
        then Some (List.filter (fun (label, _) -> label <> i) idx)
        else None
      | None -> None
    )
  in
  check e

let eval (env : (string * tensor) list) (e : expr) : tensor =
  let op_fn (o : op) : float -> float -> float =
    match o with
    | Add -> ( +. )
    | Mul -> ( *. )
  in
  let op_identity (o : op) : float =
    match o with
    | Add -> 0.
    | Mul -> 1.
  in
  let rec go (e : expr) : tensor =
    match e with
    | Ident (a, labels) ->
      let t = List.assoc a env in
      Tensor.relabel_axes t labels
    | Map (o, e1, e2) ->
      let t1 = go e1 in
      let t2 = go e2 in
      let idx1 = Tensor.idx_space t1 in
      let idx2 = Tensor.idx_space t2 in
      let extra = List.filter (fun (label, _) -> not (List.mem_assoc label idx1)) idx2 in
      let idx_space = idx1 @ extra in
      let f = op_fn o in
      Tensor.init idx_space (fun idx ->
        f (Tensor.get t1 idx) (Tensor.get t2 idx)
      )
    | Fold (o, i, e) ->
      let t = go e in
      let t_idx = Tensor.idx_space t in
      let size = List.assoc i t_idx in
      let result_idx = List.filter (fun (label, _) -> label <> i) t_idx in
      let f = op_fn o in
      let base = op_identity o in
      Tensor.init result_idx (fun idx ->
        let rec fold_loop (k : int) (acc : float) : float =
          if k >= size then acc
          else
            let full_idx = (i, k) :: idx in
            fold_loop (k + 1) (f acc (Tensor.get t full_idx))
        in
        fold_loop 0 base
      )
  in
  go e

type error =
  | Parse_error
  | Dim_error
  | Init_error

let interp (env : (string * tensor) list) (s : string) : ((string * tensor) list, error) result =
  let interp_stmt env stmt =
    match Syntax.stmt_of_sexpr_opt stmt with
    | Some (Init (a, shape, expr)) -> (
        match Tensor.of_sexpr_opt shape expr with
        | Some t -> Ok ((a, t) :: env)
        | _ -> Error Init_error
      )
    | Some (Set (a, idx, e)) -> (
        let sort  = List.sort String.compare in
        match dim_check env e with
        | Some idx_space when sort idx = sort (List.map fst idx_space) ->
          let idx_space = List.map (fun x -> (x, List.assoc x idx_space)) idx in
          let t = Tensor.init idx_space (Tensor.get (eval env e)) in
          Ok ((a, t) :: env)
        | _ -> Error Dim_error
      )
    | _ -> Error Parse_error
  in
  let rec interp_prog env = function
    | [] -> Ok env
    | e :: es -> (
        match interp_stmt env e with
        | Ok env -> interp_prog env es
        | Error e -> Error e
      )
  in
  match Sexpr.list_of_string_opt s with
  | Some ss -> interp_prog env ss
  | None -> Error Parse_error
