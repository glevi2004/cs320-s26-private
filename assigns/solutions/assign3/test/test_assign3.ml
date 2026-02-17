open Assign3

let testing = true

let run cases b = if b then cases () else []

let lex_tests () =
  [
    assert (lex "X = 3 + Y" = ["X"; "="; "3"; "+"; "Y"]);
    assert (lex "XX = 3 * YY" = ["XX"; "="; "3"; "*"; "YY"]);
    assert (lex "VAR = 2 + 3" = ["VAR"; "="; "2"; "+"; "3"]);
    (* TODO: add more tests *)
  ]

let eval_tests () =
  [
    assert (eval [("X", 2); ("Y", 3)] ["3"; "+"; "Y"] = 6);
    assert (eval [("X", 4); ("Y", 12)] ["X"; "+"; "Y"] = 16);
    (* TODO: add more tests *)
  ]

let insert_tests () =
  let l = [("X", 4); ("Y", 12); ("ADDXY", 16)] in
  [
    assert (
      List.assoc "X" l = 4
      && List.assoc "X" (insert_uniq "X" 7 l) = 7);
    (* TODO: add more tests *)
  ]

let _run_tests =
  if not testing then [] else
    [
      run lex_tests true;
      run eval_tests true;
      run insert_tests true;
    ]
