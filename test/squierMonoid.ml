(** Squier's coherent completion of the theory of monoids. *)

open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let ops = [m; e]
let x = var ()
let y = var ()
let z = var ()

let m x y = app m [x;y]
let e = app e []
let monoid =
  RS.make ops
    [
      RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
      RS.Rule.make "L" (m e x) x;
      RS.Rule.make "R" (m x e) x;
    ]

let () =
  Printf.printf "%s\n\n%!" (RS.to_string ~var:Var.namer_natural monoid)

let rule_name = Utils.namer (fun (s1,s2) (s1',s2') -> RS.Path.eq s1 s1' && RS.Path.eq s2 s2')

let coherence = RS.squier monoid

let () =
  List.iter
    (fun (s1,s2) ->
       let n = rule_name (s1,s2) in
       let var = Term.Var.namer_natural () in
       let s1 = RS.Path.to_string ~var s1 in
       let s2 = RS.Path.to_string ~var s2 in
       Printf.printf "%02d: %s\n    %s\n\n%!" n s1 s2
    ) coherence

let cpres =
  let coherence = List.map (fun (p1,p2) -> RS.Loop.of_cell (RS.Zigzag.of_path p1) (RS.Zigzag.of_path p2)) coherence in
let coherence = List.mapi (fun i p -> RS.Coherence.make ("C"^string_of_int (i+1)) p) coherence in
  RS.Coherent.make monoid coherence

(* let () = RS.Coherent.view_pdf cpres *)
