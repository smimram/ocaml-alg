(* Testing Knuth-Bendix completion. *)

open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let e = app e [||]
let i = Op.make "i" 1
let x = var ()
let y = var ()
let z = var ()

let m x y = app m [|x;y|]
let i x = app i [|x|]
let groups = [
  RS.Rule.make "unit-l" (m e x) x;
  RS.Rule.make "inv-l" (m (i x) x) e;
  RS.Rule.make "assoc" (m (m x y) z) (m x (m y z));
]

let () =
  Printf.printf "%s\n\n%!" (RS.to_string groups)

let groups = RS.knuth_bendix groups

let () =
  Printf.printf "Completed:\n\n%!";
  Printf.printf "%s\n\n%!" (RS.to_string groups)

