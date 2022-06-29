(** Completion of the theory of monoids. *)

open Alg
open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let ops = [m; e]
let x = var ()
let y = var ()
let z = var ()

let m x y = app m [x;y]
let e = app e []
let mon =
  RS.make ops
    [
      RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
      RS.Rule.make "L" (m e x) x;
      RS.Rule.make "R" (m x e) x;
    ]

let gt = LPO.gt (<=)

let mon = RS.orient ~gt mon

let () = Printf.printf "# Theory\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural mon)

let mon = RS.knuth_bendix ~gt
    (* ~callback:(fun rs -> Printf.printf "## KB\n\n%s\n\n" (RS.to_string ~var:Var.namer_natural rs)) *)
    mon

let () = Printf.printf "# Completion\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural mon)

let () =
  Printf.printf "# Coherent completion\n\n%!";
  let coh = RS.squier mon in
  List.iter
    (fun (s1,s2) ->
       let var = Term.Var.namer_natural () in
       let s1 = RS.Path.to_string ~var s1 in
       let s2 = RS.Path.to_string ~var s2 in
       Printf.printf "%s\n%s\n\n%!" s1 s2
    ) coh

