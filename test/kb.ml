(* Testing Knuth-Bendix completion. *)

open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let e = app e [||]
let i = Op.make ~weight:1 "i" 1
let x = var ()
let y = var ()
let z = var ()

let m x y = app m [|x;y|]
let i x = app i [|x|]

let monoids = [
  RS.Rule.make "unit-l" (m e x) x;
  RS.Rule.make "unit-r" (m e x) x;
  RS.Rule.make "assoc" (m (m x y) z) (m x (m y z));
]
let () = Printf.printf "monoids\n\n%s\n\n%!" (RS.to_string monoids)
let monoids = RS.knuth_bendix monoids
let () = Printf.printf "completion\n\n%s\n\n%!" (RS.to_string monoids)


let groups = [
  RS.Rule.make "E" (m e x) x;
  RS.Rule.make "I" (m (i x) x) e;
  RS.Rule.make "A" (m (m x y) z) (m x (m y z));
]
let () = Printf.printf "groups\n\n%s\n\n%!" (RS.to_string groups)
let groups = RS.knuth_bendix groups
let () = Printf.printf "completion\n\n%s\n\n%!" (RS.to_string groups)

