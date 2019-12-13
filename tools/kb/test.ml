open Term

let parse_rs syms rules =
  let s = syms in
  let s = String.split_on_char ',' s in
  let s = List.map (fun s -> String.split_on_char ':' s) s in
  let w = ref 0 in
  let s = List.map (function (f::n::_) -> decr w; Term.Op.make ~weight:(!w) f (int_of_string n) | _ -> failwith "unknown arity") s in
  ParserRefs.syms := s;
  let rs = Parser.main Lexer.token (Lexing.from_string rules) in
  let name =
    let n = ref 0 in
    fun () -> incr n; "r" ^ string_of_int !n
  in
  List.map (fun (s,t) -> RS.Rule.make (name ()) s t) rs

let () =
  let rs = parse_rs "i:1,e:0,m:2" "m(m(x,y),z)=m(x,m(y,z)) m(i(x),x) = e m(e,x) = x" in
  Printf.printf "rs ***\n%s\n\n%!" (RS.to_string rs);
  let rs = RS.knuth_bendix rs in
  Printf.printf "kb ***\n%s\n\n%!" (RS.to_string rs);
