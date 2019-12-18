(* Testing Squier completion *)

open Extlib
open Term

let ts_m a = Printf.sprintf "m(%s,%s)" (List.nth a 0) (List.nth a 1)
let ts_e a = "e()"
let ts_i a = Printf.sprintf "i(%s)" (List.hd a)

let ts_m a = Printf.sprintf "(%s\\times %s)" (List.nth a 0) (List.nth a 1)
(* let ts_m a = Printf.sprintf "%s\\times %s" (List.nth a 0) (List.nth a 1) *)
let ts_e a = "1"
let ts_i a = Printf.sprintf "\\overline{%s}" (List.hd a)

let m = Op.make ~to_string:ts_m "m" 2
let e = Op.make ~to_string:ts_e "e" 0
let i = Op.make ~to_string:ts_i "i" 1
let ops = [m; e; i]
let e = app e []
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
  let t = Term.parse ops "m(m(x,y),z)" in
  Printf.printf "term: %s\n%!" (Term.to_string t)

let m x y = app m [x;y]
let i x = app i [x]
let groups =
  RS.make ops
    [
      RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
      RS.Rule.make "L" (m e x) x;
      RS.Rule.make "R" (m x e) x;
      RS.Rule.make "E"  (i e) e;
      RS.Rule.make "I" (m (i x) x) e;
      RS.Rule.make "J" (m x (i x)) e;
      RS.Rule.make "N" (i (i x)) x;
      RS.Rule.make "T" (m (i x) (m x y)) y;
      RS.Rule.make "U" (m x (m (i x) y)) y;
      RS.Rule.make "H" (i (m x y)) (m (i y) (i x))
    ]

let () =
  Printf.printf "%s\n\n%!" (RS.to_string groups)

let hdef' = RS.Zigzag.parse groups "H(x,y)"
let hdef =
  RS.Zigzag.parse groups
    "R(i(m(x,y)))-.\
     m(i(m(x,y)),J(x))-.\
     A(i(m(x,y)),x,i(x))-.\
     m(m(i(m(x,y)),x),L(i(x)))-.\
     m(m(i(m(x,y)),x),m(J(y),i(x)))-.\
     A(m(i(m(x,y)),x),m(y,i(y)),i(x))-.\
     m(A(m(i(m(x,y)),x),y,i(y)),i(x))-.\
     m(m(A(i(m(x,y)),x,y),i(y)),i(x)).\
     m(m(I(m(x,y)),i(y)),i(x)).\
     m(L(i(y)),i(x))
"

let () =
  let var = Var.namer_natural() in
  Printf.printf "H = %s : %s -> %s\n%!" (RS.Zigzag.to_string ~var hdef) (to_string ~var (RS.Zigzag.source hdef)) (to_string ~var (RS.Zigzag.target hdef))

let rule_name = Utils.namer (fun (s1,s2) (s1',s2') -> RS.Path.eq s1 s1' && RS.Path.eq s2 s2')
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

let rule_name = Utils.namer RS.Zigzag.eq
(* let rule_name = Utils.namer (=) *)

let coherence = List.map (fun (p1,p2) -> RS.Zigzag.globe (RS.Zigzag.of_path p1) (RS.Zigzag.of_path p2)) coherence

let () =
  Printf.printf "\n****** zigzag *****\n\n%!";
  List.iter
    (fun p ->
       let n = rule_name p in
       let var = Term.Var.namer_natural () in
       let p = RS.Zigzag.to_string ~var p in
       Printf.printf "%02d: %s\n\n%!" (n+1) p
    ) coherence

let () =
  let cpres =
    let coherence = List.mapi (fun i p -> "C"^string_of_int (i+1), p) coherence in
    RS.Coherent.make groups coherence
  in
  (* let cpres = RS.Coherent.elim_rule cpres "E_r" "C36" in *)
  (* let cpres = RS.Coherent.elim_rule cpres "I_r" "C16" in *)
  let cpres = RS.Coherent.add_coherence cpres "CH" (RS.Zigzag.globe hdef' hdef) in
  let cpres = RS.Coherent.elim_rule cpres "E" "C12" in
  let cpres = RS.Coherent.elim_rule cpres "N" "C30" in
  let cpres = RS.Coherent.elim_rule cpres "T" "C5" in
  let cpres = RS.Coherent.elim_rule cpres "U" "C7" in
  let cpres = RS.Coherent.elim_rule cpres "H" "CH" in
  Printf.printf "================ eliminated:\n%s\n%!" (RS.Coherent.to_string ~var:Var.namer_natural cpres);
  RS.Coherent.view_pdf cpres

(* for Im:
   add K36 : i(m(x62,x63)) -> m(i(x63),i(x62))
   m(i(m(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))),i(m(i(m(x61,i(x62))),x61))) -K35(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))-> i(m(x62,x63))
   m(i(m(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))),i(m(i(m(x61,i(x62))),x61))) -m(i(K7(x61,x62,x63)),i(m(i(m(x61,i(x62))),x61)))-> m(i(x63),i(m(i(m(x61,i(x62))),x61))) -m(i(x63),i(K35(x61,i(x62))))-> m(i(x63),i(i(i(x62)))) -m(i(x63),i(K23(x62)))-> m(i(x63),i(x62))
*)
