open OUnit2
open Interp3


(* ----- type_of_expr tests -----
   Each case parse an input string as an expr and compares the inferred principal type scheme against an expected value. *)
let type_of_tests =
  let _t expected input =
    assert_equal expected (type_of_expr Env.empty (parse_expr input))
  in
  "type_of tests" >:::
  [
    (* int literal has type int with no quantifiers *)
    "int literal" >:: (fun _ ->
      _t (Ok ([], TInt)) "42");

    (* string literal has type string *)
    "string literal" >:: (fun _ ->
      _t (Ok ([], TString)) "\"hi\"");

    (* unit literal has type unit *)
    "unit literal" >:: (fun _ ->
      _t (Ok ([], TUnit)) "()");

    (* + returns int *)
    "addition has type int" >:: (fun _ ->
      _t (Ok ([], TInt)) "1 + 2");

    (* identity is polymorphic 'a -> 'a *)
    "identity is polymorphic" >:: (fun _ ->
      _t (Ok (["a"], TFun (TParam "a", TParam "a"))) "fun x -> x");

    (* applying id at int specialises 'a to int *)
    "id 5 has type int" >:: (fun _ ->
      _t (Ok ([], TInt)) "(fun x -> x) 5");

    (* compose: ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
    "compose principal type" >:: (fun _ ->
      _t
        (Ok (["a";"b";"c"],
             TFun (TFun (TParam "a", TParam "b"),
                   TFun (TFun (TParam "b", TParam "c"),
                         TFun (TParam "a", TParam "c")))))
        "fun g -> fun f -> fun x -> f (g x)");

    (* written-problem expr: ('a -> int) -> ('a -> 'a) -> 'a -> int *)
    "written problem principal type" >:: (fun _ ->
      _t
        (Ok (["a"],
             TFun (TFun (TParam "a", TInt),
                   TFun (TFun (TParam "a", TParam "a"),
                         TFun (TParam "a", TInt)))))
        "fun f -> fun g -> fun x -> f (g x) + f x");

    (* tuple type comes from product of element types *)
    "tuple of int and bool" >:: (fun _ ->
      _t (Ok ([], TTuple [TInt; TBool])) "(1, true)");

    (* if branches must agree, result is that branch type *)
    "if expression returns int" >:: (fun _ ->
      _t (Ok ([], TInt)) "if true then 1 else 2");
  ]


(* ----- eval_expr tests -----
   Each case parses input as an expr and checks the runtime value it evals to. The last two use assert_raises to check exceptions. *)
let eval_tests =
  let _t expected input =
    assert_equal expected (eval_expr Env.empty (parse_expr input))
  in
  "eval tests" >:::
  [
    (* int literal evaluates to itself *)
    "int literal" >:: (fun _ ->
      _t (VInt 7) "7");

    (* string literal *)
    "string literal" >:: (fun _ ->
      _t (VString "abc") "\"abc\"");

    (* arithmetic *)
    "addition" >:: (fun _ ->
      _t (VInt 5) "2 + 3");

    "multiplication" >:: (fun _ ->
      _t (VInt 6) "2 * 3");

    (* short-circuit &&, the rhs is not evaluated when lhs is false *)
    "&& short-circuits on false" >:: (fun _ ->
      _t (VBool false) "false && true");

    (* short-circuit ||, the rhs is not evaluated when lhs is true *)
    "|| short-circuits on true" >:: (fun _ ->
      _t (VBool true) "true || false");

    (* string concat *)
    "string concat" >:: (fun _ ->
      _t (VString "ab") "\"a\" ^ \"b\"");

    (* if true picks the then branch *)
    "if true picks then branch" >:: (fun _ ->
      _t (VInt 1) "if true then 1 else 2");

    (* if false picks the else branch *)
    "if false picks else branch" >:: (fun _ ->
      _t (VInt 2) "if false then 1 else 2");

    (* tuple values *)
    "tuple values" >:: (fun _ ->
      _t (VTuple [VInt 1; VInt 2; VInt 3]) "(1, 2, 3)");

    (* let binding *)
    "let binding" >:: (fun _ ->
      _t (VInt 5) "let x = 5 in x");

    (* function application *)
    "fun application" >:: (fun _ ->
      _t (VInt 11) "(fun x -> x + 1) 10");

    (* recursive factorial via let rec *)
    "let rec factorial" >:: (fun _ ->
      _t (VInt 120)
        "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5");

    (* match selects the first arm whose pattern matches *)
    "match on int literals" >:: (fun _ ->
      _t (VString "two")
        "match 2 with | 1 -> \"one\" | 2 -> \"two\" | _ -> \"other\"");

    (* dividing by zero raises Div_by_zero at the operator's position *)
    "div by zero raises" >:: (fun _ ->
      assert_raises
        (Div_by_zero ((parse_expr "1 / 0").pos))
        (fun () -> eval_expr Env.empty (parse_expr "1 / 0")));

    (* assert false raises Assert_fail at the assert's position *)
    "assert false raises" >:: (fun _ ->
      assert_raises
        (Assert_fail ((parse_expr "assert false").pos))
        (fun () -> eval_expr Env.empty (parse_expr "assert false")));
  ]


let suite =
  "interp3 test suite" >:::
  [
    type_of_tests;
    eval_tests;
  ]

let _run = run_test_tt_main suite
