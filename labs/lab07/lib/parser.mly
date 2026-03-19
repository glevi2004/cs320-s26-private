
%token EOF

%start<Ast.sexpr> prog

%%

prog:
  | EOF { assert false }
