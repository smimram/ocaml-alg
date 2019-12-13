let parse_rs syms rules =
  let s = syms in
  let s = String.split_on_char ',' s in
  let s = List.map (fun s -> String.split_on_char ':' s) s in
  let w = ref 0 in
  let s = List.map (function (f::n::_) -> decr w; Term.Op.make ~weight:(!w) f (int_of_string n) | _ -> failwith "unknown arity") s in
  ParserRefs.syms := s;
  Parser.main Lexer.token (Lexing.from_string rules)

let () =
  let rs = parse_rs "m:2" "m(m(x,y),z)=m(x,m(y,z))" in
  ignore rs
