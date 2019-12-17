(* Testing Squier completion *)

open Extlib
open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let i = Op.make "i" 1
let ops = [m; e; i]
let e = app e [||]
let x = var ()
let y = var ()
let z = var ()
(*
let assoc_l = app m [|app m [|x;y|];z|]
let assoc_r = app m [|x; app m [|y;z|]|]
let unit_l = app m [|e; x|]
let unit_c = x
let unit_r = app m [|x; e|]
let rs = [
    RS.Rule.make "assoc" assoc_l assoc_r;
    RS.Rule.make "unit-l" unit_l unit_c;
    RS.Rule.make "unit-r" unit_r unit_c
  ]
let () =
  let t = app m [|app m [|app m [|x; y|]; z|]; var ()|]
  Printf.printf "t: %s\n%!" (to_string t);
  Printf.printf "t^: %s\n\n%!" (RS.Path.to_string (RS.normalize rs t));
 *)

let () =
  let vars = ref [] in
  let t = Term.parse ops vars "m(m(x,y),z)" in
  Printf.printf "term: %s\n%!" (Term.to_string t)

let m x y = app m [|x;y|]
let i x = app i [|x|]
let groups = [
  RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
  RS.Rule.make "Eₗ" (m e x) x;
  RS.Rule.make "Eᵣ" (m x e) x;
  RS.Rule.make "E"  (i e) e;
  RS.Rule.make "Iₗ" (m (i x) x) e;
  RS.Rule.make "Iᵣ" (m x (i x)) e;
  RS.Rule.make "Iᵢ" (i (i x)) x;
  RS.Rule.make "I₁" (m (i x) (m x y)) y;
  RS.Rule.make "I₂" (m x (m (i x) y)) y;
  RS.Rule.make "Iₘ" (i (m x y)) (m (i y) (i x))
]

let () =
  Printf.printf "%s\n\n%!" (RS.to_string groups)

(* let () = *)
  (* let r = RS.Rule.make "r1" (m e x) x in *)
  (* let r' = RS.Rule.refresh r in *)
(* assert (RS.Rule.eq r r') *)

(* let rule_name = Utils.namer (fun (s1,s2) (s1',s2') -> RS.Path.eq s1 s1' && RS.Path.eq s2 s2') *)
(* TODO: equality does not seem to be working... *)
let rule_name = Utils.namer (=)

let coherence = RS.squier groups

let () =
  List.iter
    (fun (s1,s2) ->
       let n = rule_name (s1,s2) in
       let var = Term.Var.namer_natural () in
       let s1 = RS.Path.to_string ~var s1 in
       let s2 = RS.Path.to_string ~var s2 in
       Printf.printf "%02d: %s\n    %s\n\n%!" (n+1) s1 s2
    ) coherence

let rule_name = Utils.namer (=)

let coherence = List.map (fun (p1,p2) -> RS.Zigzag.of_path p1, RS.Zigzag.of_path p2) coherence

let () =
  Printf.printf "\n****** zigzag *****\n\n%!";
  List.iter
    (fun (p1,p2) ->
       let n = rule_name (p1,p2) in
       let var = Term.Var.namer_natural () in
       let s1 = RS.Zigzag.to_string ~var p1 in
       let s2 = RS.Zigzag.to_string ~var p2 in
       Printf.printf "%02d: %s\n    %s\n\n%!" (n+1) s1 s2
    ) coherence

let () =
  let r = RS.find_rule groups "Eᵣ" in
  let c = List.nth 35 coherence in
  let var = Term.Var.namer_natural () in
  Printf.printf "eliminating Eᵣ in %s / %s\n%!" (RS.Zigzag.to_string ~var (fst c)) (RS.Zigzag.to_string ~var (snd c));
  let v = RS.Zigzag.value r c in
  Printf.printf "(%s) => %s\n%!" (RS.Rule.to_string ~var r) (RS.Zigzag.to_string ~var v)
    
