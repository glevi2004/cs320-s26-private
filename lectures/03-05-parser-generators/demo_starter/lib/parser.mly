
%token EOF

%start<Utils.prog> prog

%%

prog: EOF { assert false }
