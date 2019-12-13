{
open Lexing
open Parser
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "^" { POW }
  | "(" { LPAR }
  | ")" { RPAR }
  | (['0'-'9']+ as n) { INT (int_of_string n) }
  | (['a'-'z''A'-'Z'] as c) { CHAR c }
  | space+ { token lexbuf }
  | eof { EOF }
