open Assign2

let testing = true

let run cases b = if b then cases () else []

let eval_tests () =
  [
    assert (interp "242" = 242);
    assert (interp "65 + 44" = 109);
    assert (interp "65 * (44 + 4)" = 65 * (44 + 4));
    (* Test precedence: like * has higher precedence than + *)
    assert (interp "1 + 2 * 3" = 7);
    assert (interp "2 * 3 + 4" = 10);
    (* Test left associativity *)
    assert (interp "1 - 2 - 3" = (1 - 2) - 3);
    assert (interp "10 / 2 / 5" = (10 / 2) / 5);
    assert (interp "1 + 2 - 3" = (1 + 2) - 3);
    assert (interp "6 * 2 / 3" = (6 * 2) / 3);
    (* Test parentheses override precedence *)
    assert (interp "(1 + 2) * 3" = 9);
    assert (interp "3 * (1 + 2)" = 9);
    (* Test nested parentheses *)
    assert (interp "((1 + 2))" = 3);
    assert (interp "(1 + (2 * 3))" = 7);
    (* Test complex expressios *)
    assert (interp "1 + 2 * 3 + 4" = 1 + 2 * 3 + 4);
    assert (interp "(1 + 2) * (3 + 4)" = (1 + 2) * (3 + 4));
  ]

let _run_tests =
  if not testing then [] else
    [
      run eval_tests true;
    ]
