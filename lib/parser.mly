%{
%}

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token LET FUN IN FORALL
%token LPAREN RPAREN LBRACKET RBRACKET
%token ARROW ASSIGN EQUALS NOTEQUALS COMMA
%token PLUS MINUS STAR SLASH
%token EOF

%left EQUALS NOTEQUALS
%left PLUS MINUS
%left STAR SLASH
%nonassoc UMINUS

%right ARROW

%start <Expr.t> expr_eof
%start <Type.t> ty_eof

%%

expr_eof:
  e = expr EOF { e }

expr:
  | LPAREN e = expr RPAREN { e }
  | name = IDENT { Expr.Var name }
  | LET name = IDENT ASSIGN e1 = expr IN e2 = expr { Expr.Let (name, e1, e2) }
  | lit { $1 }
  | bin { $1 }
  | neg { $1 }

lit:
  | i = INT { Expr.Lit (LInt i) }
  | s = STRING { Expr.Lit (LString s) }

bin:
  | e1 = expr PLUS e2 = expr { Bin (Expr.Add, e1, e2) }
  | e1 = expr MINUS e2 = expr { Bin (Expr.Sub, e1, e2) }
  | e1 = expr STAR e2 = expr { Bin (Expr.Mul, e1, e2) }
  | e1 = expr SLASH e2 = expr { Bin (Expr.Div, e1, e2) }
  | e1 = expr EQUALS e2 = expr { Bin (Expr.Eq, e1, e2) }
  | e1 = expr NOTEQUALS e2 = expr { Bin (Expr.NotEq, e1, e2) }

neg:
  MINUS e = expr %prec UMINUS { Expr.Neg e }

ty_eof:
  t = ty EOF { t }

ty:
  | i = IDENT { Type.Con i }
  | LPAREN RPAREN { Type.Unit }
  | t1 = ty ARROW t2 = ty { Type.Arr ([t1], t2) }
  | t1 = ty_comma_list ARROW t2 = ty { Type.Arr (t1, t2) }

ty_comma_list:
  LPAREN tys = separated_list(COMMA, ty) RPAREN { tys }
