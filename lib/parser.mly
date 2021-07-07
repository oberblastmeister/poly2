%{
%}

%token <string> IDENT
%token <string> VARIDENT
%token <string> STRING
%token <int> INT
%token LET FUN IN FORALL
%token LPAREN RPAREN LBRACKET RBRACKET
%token ARROW ASSIGN EQUALS NOTEQUALS COMMA DOT
%token PLUS MINUS STAR SLASH
%token EOF

%left EQUALS NOTEQUALS
%left PLUS MINUS
%left STAR SLASH
%nonassoc UMINUS

%right ARROW

%start <Expr.t> expr_eof
%start <Type.t> ty_eof
%start <Type.forall_naive> ty_forall_eof

%%

expr_eof:
  expr EOF { $1 }

ty_forall_eof:
  ty_forall EOF { $1 }

ty_eof:
  ty EOF { $1 }

expr:
  | LPAREN e = expr RPAREN { e }
  | name = IDENT { Expr.Var name }
  | LET name = IDENT ASSIGN e1 = expr IN e2 = expr { Expr.Let (name, e1, e2) }
  | func { $1 }
  | call { $1 }
  | lit { $1 }
  | bin { $1 }
  | neg { $1 }

func:
  | FUN args = ident_list ARROW ret = expr { Expr.Fun (args, ret) }

ident_list:
  | nonempty_list(IDENT) { $1 }

call:
  | e = expr LPAREN args = separated_list(COMMA, expr) RPAREN { Call (e, args) }

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

ty:
  | i = IDENT { Type.Con i }
  | LPAREN RPAREN { Type.Unit }
  | t1 = ty ARROW t2 = ty { Type.Arr ([t1], t2) }
  | t1 = ty_comma_list ARROW t2 = ty { Type.Arr (t1, t2) }

ty_comma_list:
  LPAREN tys = separated_list(COMMA, ty) RPAREN { tys }

ty_forall:
  | ty { { forall = []; ty = $1 } }
  | FORALL tcons = ident_list DOT t = ty { { forall = tcons; ty = t } }
