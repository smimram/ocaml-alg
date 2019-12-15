(* Testing Squier completion *)

open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let e = app e [||]
let i = Op.make "i" 1
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

let m x y = app m [|x;y|]
let i x = app i [|x|]
let groups = [
  RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
  RS.Rule.make "Eₗ" (m e x) x;
  RS.Rule.make "Eᵣ" (m x e) x;
  RS.Rule.make "E"  (i e) e;
  RS.Rule.make "Iₗ" (m (i x) x) e;
  RS.Rule.make "Iᵣ" (m x (i x)) e;
  RS.Rule.make "I²" (i (i x)) x;
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

let () =
  List.iter
    (fun (s1,s2) ->
       let n = rule_name (s1,s2) in
       Printf.printf "%02d: %s\n    %s\n\n%!" n (RS.Path.to_string s1) (RS.Path.to_string s2)
    ) (RS.squier groups)

(*
let () =
  Printf.printf "Possible homotopical reductions:\n\n";
  List.iter
    (fun (s1,s2) ->
       let tr = (RS.Path.toplevel_rules s1)@(RS.Path.toplevel_rules s2) in
       if tr <> [] then
         let tr = String.concat " " (List.map RS.Rule.to_string tr) in
         let rr1 = RS.Path.rules s1 in
         let rr2 = RS.Path.rules s2 in
         let rr1 = String.concat ", " (List.map RS.Rule.name rr1) in
         let rr2 = String.concat ", " (List.map RS.Rule.name rr2) in
         Printf.printf "%02d: %s\n    %s / %s\n    %s\n    %s\n\n%!" (rule_name (s1,s2)) tr rr1 rr2 (RS.Path.to_string s1) (RS.Path.to_string s2)
    ) (RS.squier groups)
*)
