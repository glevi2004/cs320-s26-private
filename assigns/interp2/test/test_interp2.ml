open OUnit2
open Interp2


let type_of_tests =
  let t expected input =
    assert_equal expected (type_of_expr Env.empty (parse_expr input))
  in
  let ok input = match type_of_expr Env.empty (parse_expr input) with
    | Ok _ -> true | Error _ -> false
  in
  let bad input = not (ok input) in
  "type_of tests" >:::
  [
    "unit" >:: (fun _ -> t (Ok TUnit) "()");
    "fun" >:: (fun _ -> t (Ok (TFun (TInt, TInt))) "fun (x : int) -> x");
    "if" >:: (fun _ -> t (Ok TInt) "if true then 1 else 2");
    "nil" >:: (fun _ -> t (Ok TInt_list) "[]");
    "cons" >:: (fun _ -> t (Ok TInt_list) "1 :: 2 :: []");
    "tuple" >:: (fun _ -> t (Ok (TTuple [TInt; TBool])) "(1, true)");
    "negate" >:: (fun _ -> t (Ok TInt) "- 5");
    "assert" >:: (fun _ -> t (Ok TUnit) "assert true");
    "app curried" >::
    (fun _ -> t (Ok TInt) "(fun (x : int) (y : int) -> x + y) 1 2");
    "let rec" >::
    (fun _ -> t (Ok TInt) "let rec f (x : int) : int = x in f 1");
    "match list" >::
    (fun _ -> t (Ok TInt) "match [] with | [] -> 0 | x :: _ -> x");
    "match tuple" >::
    (fun _ -> t (Ok TInt) "match (1, 2) with | (a, b) -> a + b");
    "bad: assert non-bool" >:: (fun _ -> assert_bool "" (bad "assert 1"));
    "bad: 1 + true" >:: (fun _ -> assert_bool "" (bad "1 + true"));
    "bad: if branches differ" >:: (fun _ -> assert_bool "" (bad "if true then 1 else true"));
    "bad: if cond not bool" >:: (fun _ -> assert_bool "" (bad "if 1 then 1 else 2"));
    "bad: app non-fun" >:: (fun _ -> assert_bool "" (bad "1 2"));
    "bad: too many args" >::
    (fun _ -> assert_bool "" (bad "(fun (x : int) -> x) 1 2"));
    "bad: rec without annot" >::
    (fun _ -> assert_bool "" (bad "let rec f (x : int) = x in f 1"));
    "bad: rec without arg" >::
    (fun _ -> assert_bool "" (bad "let rec x : int = 1 in x"));
    "bad: pattern bound twice" >::
    (fun _ -> assert_bool "" (bad "match (1, 2) with | (a, a) -> a"));
    "bad: pattern type mismatch" >::
    (fun _ -> assert_bool "" (bad "match 1 with | true -> 1 | _ -> 0"));
  ]

let eval_tests =
  let t expected input =
    assert_equal expected (eval_expr Env.empty (parse_expr input))
  in
  "eval tests" >:::
  [
    "unit" >:: (fun _ -> t VUnit "()");
    "let+fun" >::
    (fun _ -> t (VInt 2) "let x = 2 in (fun (y : bool) -> x) true");
    "negate" >:: (fun _ -> t (VInt (-3)) "- 3");
    "arith" >:: (fun _ -> t (VInt 7) "1 + 2 * 3");
    "if true" >:: (fun _ -> t (VInt 1) "if 1 < 2 then 1 else 0");
    "and short" >:: (fun _ -> t (VBool false) "false && (1 / 0 = 0)");
    "or short" >:: (fun _ -> t (VBool true) "true || (1 / 0 = 0)");
    "match nil" >::
    (fun _ -> t (VInt 0) "match [] with | [] -> 0 | x :: _ -> x");
    "match cons" >::
    (fun _ -> t (VInt 1) "match 1 :: [] with | [] -> 0 | x :: _ -> x");
    "match cons sum" >::
    (fun _ -> t (VInt 3)
       "match 1 :: 2 :: [] with | x :: y :: _ -> x + y | _ -> 0");
    "match tuple" >::
    (fun _ -> t (VInt 5) "match (2, 3) with | (a, b) -> a + b");
    "match falls through" >::
    (fun _ -> t (VBool false) "match 5 with | 0 -> true | _ -> false");
    "match int literal" >::
    (fun _ -> t (VBool true) "match 0 with | 0 -> true | _ -> false");
    "let rec fact" >::
    (fun _ -> t (VInt 120)
       "let rec fact (n : int) : int = if n <= 0 then 1 else n * fact (n - 1) in fact 5");
    "match fail raises" >::
    (fun _ ->
       let raised =
         try
           let _ = eval_expr Env.empty (parse_expr "match 1 with | 0 -> 0") in
           false
         with Match_fail _ -> true
       in
       assert_bool "expected Match_fail" raised);
  ]

let suite =
  "interp1 test suite" >:::
  [
    type_of_tests;
    eval_tests;
  ]

let _run = run_test_tt_main suite
