%{
open Pol

let rec muls = function
  | [] -> One
  | [x] -> x
  | x::l -> Mul (x, muls l)

let rec pow p = function
  | 0 -> One
  | 1 -> p
  | n -> Mul (p, pow p (n-1))
%}

%token <int> INT
%token ADD SUB MUL POW
%token LPAR RPAR
%token <char> CHAR
%token EOF

%left ADD SUB
%left MUL
%nonassoc POW
%nonassoc UMINUS

%start main
%type <Pol.t> main
%%

main:
  | expr EOF { $1 }

expr:
  | expr ADD expr { Add ($1, $3) }
  | expr MUL expr { Mul ($1, $3) }
  | expr SUB expr { Sub ($1, $3) }
  | expr POW INT { pow $1 $3 }
  | chars { muls $1 }
  | INT { if $1 = 0 then Zero else if $1 = 1 then One else failwith "unexpected number" }
  | SUB expr %prec UMINUS { Neg $2 }
  | LPAR expr RPAR { $2 }
;

chars:
  | CHAR chars { (Gen $1)::$2 }
  | CHAR { [Gen $1] }
;
