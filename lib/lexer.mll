{
open Parser
open Core

exception SyntaxError
}

let white = [' ' '\t' '\r' '\n']
let ident = ['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let digit = ['0'-'9']
let integer = digit+

rule token = parse
	| white { token lexbuf }

  | ident { IDENT (Lexing.lexeme lexbuf) }
  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }

  | "fun" { FUN }
  | "let" { LET }
  | "in" { IN }
  | "forall" { FORALL }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }

  | "->" { ARROW }
  | '=' { ASSIGN }
  | "==" { EQUALS }
  | "!=" { NOTEQUALS }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { SLASH }
  | '*' { STAR }
  | ',' { COMMA }

  | eof { EOF }
  | _ { raise SyntaxError }
