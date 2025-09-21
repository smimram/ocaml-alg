(* Knuth-Bendix completion of categorical combinators. *)

open Alg
open Term

let abs = Op.make "L" 1
let app = Op.make "o" 2
let pa = Op.make "p" 2
let fst = Op.make "fst" 0
let snd = Op.make "snd" 0
let id = Op.make "id" 0

let x = var ()
let y = var ()
let z = var ()

let abs x = Term.app abs [x]
let app x y = Term.app app [x;y]
let pa x y = Term.app pa [x;y]
let fst = Term.app fst []
let snd = Term.app snd []

(*
let monoids =
  RS.make [mm; ee]
    [
      RS.Rule.make "El" (m e x) x;
      RS.Rule.make "Er" (m x e) x;
      RS.Rule.make "A" (m (m x y) z) (m x (m y z));
    ]
let () = Printf.printf "monoids\n\n%s\n\n%!" (RS.to_string monoids)
let monoids = RS.knuth_bendix monoids
let () = Printf.printf "completion\n\n%s\n\n%!" (RS.to_string monoids)

let groups =
  RS.make [mm; ee; ii]
    [
      RS.Rule.make "E" (m e x) x;
      RS.Rule.make "I" (m (i x) x) e;
      RS.Rule.make "A" (m (m x y) z) (m x (m y z));
    ]
let () = Printf.printf "groups\n\n%s\n\n%!" (RS.to_string groups)
let groups = RS.knuth_bendix groups
let () = Printf.printf "completion\n\n%s\n\n%!" (RS.to_string groups)
*)
   
(* Tarski's presentations of groups with one rule. *)
(*
let dd = Op.make "d" 2
let d x y = app dd [x;y]
let x = var ()
let y = var ()
let z = var ()
let groups =
  RS.make [dd]
    [
      RS.Rule.make "D"
        (d
           x
           (d
              (d
                 (d (d x x) y)
                 z
              )
              (d
                 (d
                    (d x x)
                    x
                 )
                 z
              )
           )
        )
        y
    ]
let () = Printf.printf "groups\n\n%s\n\n%!" (RS.to_string groups)
let groups = RS.knuth_bendix groups
let () = Printf.printf "completion\n\n%s\n\n%!" (RS.to_string groups)
*)
