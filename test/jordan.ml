(* Testing non-commutative Jordan algebras. *)

open Term

let mm = Op.make "m" 2
let x = var ()
let y = var ()

let m x y = app mm [x;y]

let ja =
  RS.make [mm]
    [
      RS.Rule.make "A" (m (m x y) (m x x)) (m x (m y (m x x)));
    ]
let () = Printf.printf "Jordan algebras\n\n%s\n\n%!" (RS.to_string ja)
(* let ja = RS.knuth_bendix ja *)
(* let () = Printf.printf "completion\n\n%s\n\n%!" (RS.to_string ja) *)
