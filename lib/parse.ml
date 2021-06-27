open Parser

type lexer = Lexing.lexbuf -> Parser.token

type 'a parser = lexer -> Lexing.lexbuf -> 'a

let parse p s = p Lexer.token (Lexing.from_string s)

let parse_expr = parse Parser.expr_eof
