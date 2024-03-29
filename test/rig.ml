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

let com = RS.Rule .make "C" (p x y) (p y x)

module X = struct
  include Alphabet.Int
  let to_string n = Printf.sprintf "x%d" n
end
module M = Algebra.Free(Field.Float)(Monoid.Free(X))

let () =
  let module P = Interpretation.Polynomial in
  let op f x =
    let x i = P.var x.(i) in
    let (+) = P.add in
    let ( * ) = P.mul in
    let ( *~ ) = P.cmul in
    match Op.name f with
    | "p" -> P.add (2 *~ x 0 + x 1) P.one
    | "u" -> P.one
    | "m" -> x 0 * x 1
    | "e" -> P.one
    | _ -> assert false
  in
  Printf.printf "# Interpretation of generators\n\n%!";
  List.iter (fun t -> Printf.printf "%s : %s\n%!" (Term.to_string t) (P.to_string (P.interpretation op t))) [m x y; e; p x y; u];
  Printf.printf "\n# Interpretation of rules\n\n%!";
  List.iter
    (fun r ->
       let s = RS.Rule.source r in
       let t = RS.Rule.target r in
       let s' = P.interpretation op s in
       let t' = P.interpretation op t in
       Printf.printf "%s => %s : %s => %s\n  %s\n%!" (Term.to_string s) (Term.to_string t) (P.to_string s') (P.to_string t') (P.to_string (P.sub s' t'))
    ) ((RS.rules rigs)@[com]);
  Printf.printf "\n%!"

let () = Printf.printf "# Theory\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural rigs)

let gt = LPO.gt (<=)

let rigs = RS.orient ~gt rigs

let () = Printf.printf "# Oriented theory\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural rigs)

(* Now, semi-rigs *)

let rigs = RS.filter (fun r -> RS.Rule.name r <> "D") rigs

let rigs = RS.knuth_bendix ~gt ~callback:(fun rs -> Printf.printf "## KB\n\n%s\n\n" (RS.to_string ~var:Var.namer_natural rs)) rigs

let () = Printf.printf "# Completion\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural rigs)

let () =
  let s = RS.squier rigs in
  Printf.printf "# Coherence\n\n";
  List.iter
    (fun (p,q) ->
       Printf.printf "%s\n%s\n\n" (RS.Path.to_string p) (RS.Path.to_string q)
    ) s;
  Printf.printf "\n"
