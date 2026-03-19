include Utils

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let eval (e : expr) : value option =
  ignore e; assert false

let interp (s : string) : value option =
  ignore s; assert false
