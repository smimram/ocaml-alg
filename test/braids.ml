open Alg

module X = struct
  include Alphabet.Int
  let to_string n = String.make 1 (char_of_int (int_of_char 'a' + n))
end
module P = Monoid.Pres(X)

let () =
  let a = 0 in
  let b = 1 in
  let c = 2 in
  let pres =
    P.make [a;b;c] [
    "",[|a;b;a|],[|b;a;b|];
    "",[|a;b|],[|c|];
  ]
  in
  let leq = P.W.Order.deglex X.geq in
  print_endline ("presentation: " ^ P.to_string pres);
  let pres = P.orient leq pres in
  print_endline ("oriented presentation: " ^ P.to_string pres);
  let pres = P.complete leq pres in
  print_endline ("completed: " ^ P.to_string pres);
  let pres = P.reduce pres in
  print_endline ("reduced: " ^ P.to_string pres);
