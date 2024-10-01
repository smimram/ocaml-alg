(* Example from
   https://terrytao.wordpress.com/2024/09/25/a-pilot-project-in-universal-algebra-to-explore-new-ways-to-collaborate-and-use-machine-assistance/
   and
   https://mathoverflow.net/questions/450890/is-there-an-identity-between-the-commutative-identity-and-the-constant-identity/450905#450905
*)

open Alg
open Term

let m = Op.make "m" 2

let ops = [m]

let m x y = app m [x;y]

let x = var ()
let y = var ()

let rs =
  RS.make ops
    [
      RS.Rule.make "R" (m (m x x) y) (m y x);
    ]

let var = Var.namer_natural

let () = Printf.printf "# Theory\n\n%s\n\n%!" (RS.to_string ~var rs)

let gt = LPO.gt (>=)

let rs = RS.orient ~gt rs

let () = Printf.printf "# Oriented\n\n%s\n\n%!" (RS.to_string ~var rs)

let rs =
  let n = ref 0 in
  let callback rs =
    incr n;
    if !n = 2 then
      (
        Printf.printf "%s\n\n%!" (RS.to_string ~var rs);
        exit 0
      )
  in
  RS.knuth_bendix ~gt ~callback rs

let () = Printf.printf "# Completion\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural rs)
