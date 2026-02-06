open Assign1

let testing = true

let run cases b = if b then cases () else []

let sqrt_tests () =
  [
    assert (3 = sqrt 9);
    assert (4 = sqrt 10);
    assert (10 = sqrt 100);
    assert (11 = sqrt 120);
  ]

let pow_tests () =
  [
    assert (8 = pow 2 3);
    assert (3 * 3 * 3 * 3 * 3 * -1 = pow (-3) 5);
    assert (1 = pow 100 0);
    assert (0 = pow 0 23);
  ]

let split_on_ws_tests () =
  let f = split_on_ws in
  [
    assert (["a"; "bc"] = f "a bc");
    assert (["a"; "bc"] = f "       a bc     ");
    assert (["a"; "bc"] = f "a\n\n\n\n\nbc");
    assert (["-23"; "45"; "+"] = f "-23 45 +");
  ]

let eval_tests () =
  [
    assert ([1] = eval [] ["1"]);
    assert ([-23; 1] = eval [] ["1"; "-23"]);
    assert (eval [1; 2; 3] ["-1"] = [-1; 1; 2; 3]);
    assert (eval [1; 2; 3] ["-1"; "+"] = [0; 2; 3]);
    assert (eval [] ["-1"; "2"; "3"; "-"; "+"; "4"; "*"] = [-8]);
    assert (eval [1; 2; 3] ["-1"; "2"; "3"; "-"; "+"; "4"; "*"] = [-8; 1; 2; 3]);
  ]

let _run_tests =
  if not testing then [] else
    [
      run sqrt_tests true;
      run pow_tests true;
      run split_on_ws_tests true;
      run eval_tests true;
    ]
