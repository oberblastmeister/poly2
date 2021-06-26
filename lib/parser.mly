%{
open Expr
%}

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token LET FUN IN FORALL
%token LPAREN RPAREN LBRACKET RBRACKET
%token ARROW ASSIGN EQUALS NOTEQUALS COMMA
%token PLUS MINUS STAR SLASH
%token EOF

%left PLUS MINUS
%left STAR SLASH
%nonassoc UMINUS

%start expr_eof
%type <Expr.t> expr_eof

%%

expr_eof:
  | e = expr EOF { e }

expr:
  | LPAREN e = expr RPAREN { e }
  | LET name = IDENT ASSIGN e1 = expr IN e2 = expr { Let (name, e1, e2) }
  | lit { $1 }
  | bin { $1 }
  | neg { $1 }

lit:
  | i = INT { Lit (LInt i) }
  | s = STRING { Lit (LString s) }

bin:
  | e1 = expr PLUS e2 = expr { Bin (Add, e1, e2) }
  | e1 = expr MINUS e2 = expr { Bin (Sub, e1, e2) }
  | e1 = expr STAR e2 = expr { Bin (Mul, e1, e2) }
  | e1 = expr SLASH e2 = expr { Bin (Div, e1, e2) }

neg:
  MINUS e = expr %prec UMINUS { Neg e }

(* t_lit: *)
(*   | *) 
