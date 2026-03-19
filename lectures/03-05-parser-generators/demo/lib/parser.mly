%{
    open Utils
%}

%token LET
%token EQ
%token IN
%token EOF
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token LPAREN
%token RPAREN
%token<string> IDENT
%token<int> INT

%start <Utils.prog> prog

%left PLUS, MINUS
%left STAR, SLASH

%%

prog:
  | e = expr; EOF { e }

expr:
  | LET; x = var; EQ; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | e = expr1 { e }

%inline bop:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }

expr1:
  | e1 = expr1; op = bop; e2 = expr1 { Bop (op, e1, e2) }
  | n = num { Num n }
  | x = var { Var x }
  | LPAREN; e = expr; RPAREN { e }

num:
  | n = INT { n }

var:
  | x = IDENT { x }
