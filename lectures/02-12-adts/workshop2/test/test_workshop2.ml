open Workshop2
open OUnit2

let tree_printer printer (t : 'a tree) =
  let rec go indent t =
    match t with
    | Leaf x -> String.make (2 * indent) ' ' ^ printer x
    | Node2 (x, l, r) ->
      Printf.sprintf
        "%s%s\n%s\n%s"
        (String.make (2 * indent) ' ')
        (printer x)
        (go (indent + 1) l)
        (go (indent + 1) r)
    | Node3 (x, l, m, r) ->
      Printf.sprintf
        "%s%s\n%s\n%s\n%s"
        (String.make (2 * indent) ' ')
        (printer x)
        (go (indent + 1) l)
        (go (indent + 1) m)
        (go (indent + 1) r)
  in go 0 t

let reverse_tests =
  let printer t = "\n" ^ tree_printer string_of_int t ^ "\n" in
  let eq = assert_equal ~printer in
  "reverse tests" >:::
  [
    "basic" >:: (fun _ ->
        eq
          (Node2 (1, Leaf 3, Leaf 2))
          (reverse (Node2 (1, Leaf 2, Leaf 3))));
    "example" >:: (fun _ ->
        let t1 =
          Node3
            ( 1
            , Node2
                ( 2
                , Leaf 3
                , Leaf 4
                )
            , Leaf 5
            , Node2
                ( 6
                , Leaf 7
                , Leaf 8
                )
            )
        in
        let t2 =
          Node3
            ( 1
            , Node2
                ( 6
                , Leaf 8
                , Leaf 7
                )
            , Leaf 5
            , Node2
                ( 2
                , Leaf 4
                , Leaf 3
                )
            )
        in eq t2 (reverse t1));
  ]

let split_at_tests =
  let printer (a, b) = Printf.sprintf "(\"%s\", \"%s\")" a b in
  let eq = assert_equal ~printer in
  "split_at tests" >:::
  [
    "basic index" >::
    (fun _ ->
       eq
         ("abc", "def")
         (split_at 3 "abcdef"));
    "negative index" >::
    (fun _ ->
       eq
         ("", "abcdef")
         (split_at (-3) "abcdef"));
    "large index" >::
    (fun _ ->
       eq
         ("abcdef", "")
         (split_at 10 "abcdef"));
    "zero index" >::
    (fun _ ->
       eq
         ("", "abcdef")
         (split_at 0 "abcdef"));
    "length index" >::
    (fun _ ->
       eq
         ("abcdef", "")
         (split_at 6 "abcdef"));
  ]

let get_int_tests =
  let printer x =
    match x with
    | Some (n, s) -> Printf.sprintf "(\"%d\", \"%s\")" n s
    | None -> "None"
  in
  let eq = assert_equal ~printer in
  "get_int_tests" >:::
  [
    "basic some" >::
    (fun _ ->
       eq
         (Some (123, "abc"))
         (get_int "123abc"));
    "basic none" >::
    (fun _ ->
       eq
         None
         (get_int "abc123"));
    "starting space" >::
    (fun _ ->
       eq
         None
         (get_int " 123abc"));
    "ending space" >::
    (fun _ ->
       eq
         (Some (123, " abc"))
         (get_int "123 abc"));
    "negative" >::
    (fun _ ->
       eq
         (Some (-123, " abc"))
         (get_int "-123 abc"));
    "negative zero" >::
    (fun _ ->
       eq
         (Some (-0, " abc"))
         (get_int "-0 abc"));
    "entire string" >::
    (fun _ ->
       eq
         (Some (12345, ""))
         (get_int "12345"));
  ]

let eval_tests =
  let printer x =
    match x with
    | Ok x -> string_of_int x
    | Error DivByZero -> "<divByZero>"
    | Error NegExp -> "<negExp>"
  in
  let eq = assert_equal ~printer in
  "eval tests" >:::
  [
    "basic" >::
    (fun _ ->
       eq
         (Ok 9)
         (eval (Mul (Add (Int 1, Int 2), Int 3))));
    "div by zero" >::
    (fun _ ->
       eq
         (Error DivByZero)
         (eval
            (Div (Add (Int 1, Int 2), (Add (Int 1, Int (-1)))))));
    "negative exp" >::
    (fun _ ->
       eq
         (Error NegExp)
         (eval (Exp (Add (Int 1, Int 2), (Add (Int (-2), Int (-1)))))));
    "multiple Errors" >::
    (fun _ -> eq
        (Error NegExp)
        (eval
           (Sub
             ( Exp
                 ( Add (Int 1 , Int 2)
                 , Add (Int (-2), Int (-1))
                 )
             , Div (Int 1, Int 0)
             ))));
  ]

let suite =
  "workshop 2 test suite" >:::
  [
    split_at_tests;
    get_int_tests;
    reverse_tests;
    eval_tests;
  ]

let _ = run_test_tt_main suite
