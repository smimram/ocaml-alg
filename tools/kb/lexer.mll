{
open Parser
}

let space = ' ' | '\t' | '\r' | '\n'

rule token = parse
  | "=" { EQ }
  | "(" { LPAR }
  | ")" { RPAR }
  | "," { COMMA }
  | (['a'-'z']+ as s) { IDENT s }
  | space+ { token lexbuf }
  | eof { EOF }
