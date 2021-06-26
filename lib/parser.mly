%{
open Expr
%}

%token LET
%token EOF

%start expr_eof
%type <unit> expr_eof

%%

expr_eof:
  | EOF {  }
