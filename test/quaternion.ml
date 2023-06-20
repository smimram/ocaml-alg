open Alg

module X = struct
  include Alphabet.Int
  let to_string n = String.make 1 (char_of_int (int_of_char 'a' + n))
end
module P = Monoid.Pres(X)

let () =
  let a = 0 in
  let b = 1 in
  let pres =
    P.make [a;b] [
    [|a;a;a;a|],[||];
    [|a;a|],[|b;b|];
    [|a;b;a|],[|b|]
  ]
  in
  let pres = P.complete (P.W.Order.deglex X.leq) pres in
  print_endline (P.to_string pres)

let () =
  let i = 0 in
  let j = 1 in
  let k = 2 in
  let e = 3 in
  let pres =
    P.make [i;j;k;e] [
      [|i;i|],[|e|];
      [|j;j|],[|e|];
      [|k;k|],[|e|];
      [|i;j;k|],[|e|];
      [|e;e|],[||]
    ]
  in
  let pres = P.complete (P.W.Order.deglex X.leq) pres in
  print_endline (P.to_string pres)
