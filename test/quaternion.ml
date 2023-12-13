open Alg

module X = struct
  include Alphabet.Int
  let to_string n = String.make 1 (char_of_int (int_of_char 'a' + n))
end
module P = Monoid.Pres(X)

let print fmt = Printf.printf fmt

let string_of_branching (u1,((u,u'):P.Rule.t),u2) (v1,((_v,v'):P.Rule.t),v2) =
  let to_string u = if P.W.eq u P.W.one then "" else P.W.to_string u in
  let u1 = to_string u1 in
  let u = to_string u in
  let u' = to_string u' in
  let u2 = to_string u2 in
  let v1 = to_string v1 in
  let v' = to_string v' in
  let v2 = to_string v2 in
  Printf.sprintf "%s%s%s ←%s_%s- %s%s%s -%s_%s→ %s%s%s" u1 u' u2 u1 u2 u1 u u2 v1 v2 v1 v' v2

let string_of_branching (r,s) = string_of_branching r s

let study pres =
  print "# Presentation\n\n";
  print "Original:   %s\n\n" (P.to_string pres);
  let pres = P.complete (P.W.Order.deglex X.leq) pres in
  print "Completion: %s\n\n" (P.to_string pres);
  let pres = P.reduce pres in
  print "Reduction: %s\n\n" (P.to_string pres);
  let cb = P.critical_branchings pres in
  print "Branchings (%d):\n- %s\n\n" (List.length cb) (cb |> List.map string_of_branching |> String.concat "\n- ")

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
  study pres

(*
let () =
  let i = 0 in
  let j = 1 in
  let pres =
    P.make [i;j] [
      [|i;j;i|],[|j|];
      [|j;i;j|],[|i|]
    ]
  in
  study pres

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
  study pres

let () =
  let r = 0 in
  let g = 1 in
  let b = 2 in
  let pres =
    P.make [r;g;b] [
      [|b|],[|r;g|];
      [|b;g;r|],[||];
      [|g;b|],[|r|];
    ]
  in
  study pres
*)
