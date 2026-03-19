
let whitespace = [' ' '\t' '\n' '\r']+

rule read =
  parse
  | eof { Parser.EOF }
