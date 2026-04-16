
type ty =
  | Int
  | Bool
  | Fun of ty * ty
  | Param of string

type constr = ty * ty
type solution = (string * ty) list

let rec apply (s : solution) (t : ty) : ty =
  match t with
  | Int -> Int
  | Bool -> Bool
  | Fun (t1, t2) -> Fun (apply s t1, apply s t2)
  | Param x ->
    match List.assoc_opt x s with
    | Some t' -> t'
    | None -> Param x

let rec fv (t : ty) : string list =
  match t with
  | Int | Bool -> []
  | Fun (t1, t2) -> fv t1 @ fv t2
  | Param x -> [x]

let unify (cs : constr list) : solution option =
  let subst x t = apply [(x, t)] in
  let rec go cs s =
    match cs with
    | [] -> Some s
    | (t1, t2) :: rest when t1 = t2 -> go rest s
    | (Fun (a1, b1), Fun (a2, b2)) :: rest ->
      go ((a1, a2) :: (b1, b2) :: rest) s
    | (Param x, t) :: rest when not (List.mem x (fv t)) ->
      let rest' = List.map (fun (a, b) -> (subst x t a, subst x t b)) rest in
      let s' = List.map (fun (y, ty) -> (y, subst x t ty)) s in
      go rest' ((x, t) :: s')
    | (t, Param x) :: rest when not (List.mem x (fv t)) ->
      let rest' = List.map (fun (a, b) -> (subst x t a, subst x t b)) rest in
      let s' = List.map (fun (y, ty) -> (y, subst x t ty)) s in
      go rest' ((x, t) :: s')
    | _ -> None
  in
  go cs []
