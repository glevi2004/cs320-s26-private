module Utils = Utils

let parse (s : string) : Utils.prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None
