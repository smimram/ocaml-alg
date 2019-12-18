%{
open Term

let vars = ref []

let var x =
  if not (List.mem_assoc x !vars) then vars := (x, Term.var ()) :: !vars;
  List.assoc x !vars

let is_sym f = List.exists (fun g -> Op.name g = f) !ParserRefs.syms
let sym f = List.find (fun g -> Op.name g = f) !ParserRefs.syms

let app f args =
  let f = sym f in
  if List.length args <> Op.arity f then failwith ("arity mismatch for " ^ Op.name f);
  app f args
%}

%token <string> IDENT
%token LPAR RPAR COMMA EQ
%token EOF

%start main
%type <(Term.t * Term.t) list> main
%%

main:
  | rules EOF { $1 }

rules:
  | { [] }
  | rule rules { $1 :: $2 }

rule:
  | term EQ term { $1, $3 }

term:
  | IDENT LPAR terms RPAR { app $1 $3 }
  | IDENT { if is_sym $1 then app $1 [] else var $1 }
;

terms:
  | term COMMA terms { $1::$3 }
  | term { [$1] }
  | { [] }
