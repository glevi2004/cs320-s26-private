
let parse (s : string) : Ast.sexpr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None
