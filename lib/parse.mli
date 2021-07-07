type lexer = Lexing.lexbuf -> Parser.token

type 'a parser = lexer -> Lexing.lexbuf -> 'a

val parse : 'a parser -> string -> 'a

val parse_expr : string -> Expr.t

val parse_type : string -> Type.t

val parse_forall_type_with : string -> Id_supply.t -> Type.t
