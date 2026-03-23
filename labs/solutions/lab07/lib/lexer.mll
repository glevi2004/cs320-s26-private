
let whitespace = [' ' '\t' '\n' '\r']+
let atom = [^ ' ' '\t' '\n' '\r' '(' ')']+

rule read =
  parse
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | atom { Parser.ATOM (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { Parser.EOF }
