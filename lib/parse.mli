type lexer = Lexing.lexbuf -> Parser.token

type 'a parser = lexer -> Lexing.lexbuf -> 'a

val parse : 'a parser -> string -> 'a

val parse_expr : string -> Expr.t
