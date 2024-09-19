(** Normalization of types in simply typed lambda-calculus, with associativity and unitality. *)

open Alg
open Term

let a = Op.make "→" 2 (* Arrow *)
let p = Op.make "×" 2 (* Product *)
let e = Op.make "1" 0 (* Terminal object *)
let ops = [a; p; e]
let x = var ()
let y = var ()
let z = var ()

let a x y = app a [x;y]
let p x y = app p [x;y]
let e = app e []

let stl =
  RS.make ops
    [
      RS.Rule.make "L" (a (p x y) z) (a x (a y z));
      RS.Rule.make "R" (a x (p y z)) (p (a x y) (a x z));
      RS.Rule.make "L1" (a e x) x;
      RS.Rule.make "R1" (a x e) e;
      RS.Rule.make "A" (p (p x y) z) (p x (p y z));
      RS.Rule.make "PL" (p e x) x;
      RS.Rule.make "PR" (p x e) x;
    ]

let () = Printf.printf "# Theory\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural stl)

let gt = LPO.gt (>=)

let stl = RS.orient ~gt stl

let () = Printf.printf "# Oriented\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural stl)

let stl = RS.knuth_bendix ~gt
    (* ~callback:(fun rs -> Printf.printf "## KB\n\n%s\n\n" (RS.to_string ~var:Var.namer_natural rs)) *)
    stl

let () = Printf.printf "# Completion\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural stl)

let () =
  let branchings =
    RS.critical stl |> List.map fst |> List.map RS.Step.source |> List.map Term.to_string |> String.concat "\n"
  in
  Printf.printf "# Branchings\n\n%s\n\n%!" branchings

let () =
  Printf.printf "# Coherent completion\n\n%!";
  let coh = RS.squier stl in
  List.iter
    (fun (s1,s2) ->
       let var = Term.Var.namer_natural () in
       let s1 = RS.Path.to_string ~var s1 in
       let s2 = RS.Path.to_string ~var s2 in
       Printf.printf "%s\n%s\n\n%!" s1 s2
    ) coh

