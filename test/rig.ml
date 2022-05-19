(** Completion of the theory of RIGs. *)

open Alg
open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let p = Op.make "p" 2
let u = Op.make "u" 0
let ops = [m; e; p; u]
let x = var ()
let y = var ()
let z = var ()

let m x y = app m [x;y]
let e = app e []
let p x y = app p [x;y]
let u = app u []
let rigs =
  RS.make ops
    [
      RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
      RS.Rule.make "L" (m e x) x;
      RS.Rule.make "R" (m x e) x;
      RS.Rule.make "A+"  (p (p x y) z) (p x (p y z));
      RS.Rule.make "L+" (p u x) x;
      RS.Rule.make "R+" (p x u) x;
      RS.Rule.make "D" (m x (p y z)) (p (m x y) (m x z));
      RS.Rule.make "D'" (m (p x y) z) (p (m x z) (m y z));
      RS.Rule.make "N" (m x u) u;
      RS.Rule.make "N'" (m u x) u;
    ]

let () = Printf.printf "# Theory\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural rigs)

let gt = LPO.gt (<=)

let rigs = RS.orient ~gt rigs

let () = Printf.printf "# Oriented theory\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural rigs)

let rigs = RS.knuth_bendix ~gt ~callback:(fun rs -> Printf.printf "## KB\n\n%s\n\n" (RS.to_string ~var:Var.namer_natural rs)) rigs

let () = Printf.printf "# Completion\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural rigs)
