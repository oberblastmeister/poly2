{
open Lexing
open Parser
open Core

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t' '\r']
let ident = ['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let var_ident = ['\''] ident
let digit = ['0'-'9']
let integer = digit+

rule token = parse
  | "fun" { FUN }
  | "let" { LET }
  | "in" { IN }
  | "forall" { FORALL }

	| white+ { token lexbuf }
  | '\n' { next_line lexbuf; token lexbuf }
  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | var_ident { VARIDENT (Lexing.lexeme lexbuf) }
  | ident { IDENT (Lexing.lexeme lexbuf) }

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
  | '.' { DOT }

  | '"' { token_string (Buffer.create 17) lexbuf }

  | eof { EOF }
  | _ { raise (SyntaxError ("invalid token")) }

and token_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; token_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; token_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; token_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; token_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; token_string buf lexbuf }
  | '\\' '\"' { Buffer.add_char buf '\"'; token_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; token_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); token_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
