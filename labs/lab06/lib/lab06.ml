
let map (f : 'a -> 'b) (l : 'a list) : 'a list =
  ignore (f, l); assert false

let filter (f : 'a -> bool) (l : 'a list) : 'a list =
  ignore (f, l); assert false

let fold_left
    (f : 'acc -> 'a -> 'acc)
    (base : 'acc)
    (l : 'a list) : 'acc =
  ignore (f, base, l); assert false

let fold_right
    (f : 'a -> 'acc -> 'acc)
    (l : 'a list)
    (base : 'acc) : 'acc =
  ignore (f, l, base); assert false

module Matrix = struct
  type 'a t = 'a list list

  let init (dim : int * int) (f : int -> int -> 'a) : 'a t =
    ignore (dim, f); assert false

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    ignore (f, m); assert false

  let fold_left
      (f : 'acc -> 'a -> 'acc)
      (base : 'acc)
      (m : 'a t) : 'acc =
    ignore (f, base, m); assert false
end
