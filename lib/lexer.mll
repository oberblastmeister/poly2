{
open Parser
open Core

exception SyntaxError of string
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

  | '"' { read_string (Buffer.create 17) lexbuf }

  | eof { EOF }
  | _ { raise (SyntaxError ("invalid token")) }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\"' { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
