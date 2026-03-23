
%token LPAREN
%token RPAREN
%token<string> ATOM
%token EOF

%start<Ast.sexpr> prog

%%

prog:
  | e=sexpr; EOF { e }

sexpr:
  | a=ATOM { Ast.Atom a }
  | LPAREN; l=sexpr*; RPAREN { Ast.List l }
