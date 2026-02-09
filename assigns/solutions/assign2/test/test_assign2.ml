open Assign2

let testing = true

let run cases b = if b then cases () else []

let eval_tests () =
  [
    assert (eval (lex "242") = 242);
    assert (eval (lex "65 + 44") = 109);
    assert (eval (lex "65 * (44 + 4)") = 65 * (44 + 4));
    (* ADD MORE ASSERTIONS *)
  ]

let _run_tests =
  if not testing then [] else
    [
      run eval_tests true;
    ]
