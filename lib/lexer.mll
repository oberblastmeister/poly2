{
open Parser
}

let ident = ['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let digit = ['0'-'9']
let integer = digit+

rule token = parse
  | "let" { LET }
