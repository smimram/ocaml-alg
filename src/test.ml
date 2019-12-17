(* Testing Squier completion *)

open Extlib
open Term

let ts_m a = Printf.sprintf "(%s\\times %s)" (List.nth a 0) (List.nth a 1)
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
  let vars = ref [] in
  let t = Term.parse ops vars "m(m(x,y),z)" in
  Printf.printf "term: %s\n%!" (Term.to_string t)

let m x y = app m [x;y]
let i x = app i [x]
let groups = [
  RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
  RS.Rule.make "L" (m e x) x;
  RS.Rule.make "R" (m x e) x;
  RS.Rule.make "E"  (i e) e;
  RS.Rule.make "J" (m (i x) x) e;
  RS.Rule.make "I" (m x (i x)) e;
  RS.Rule.make "I_i" (i (i x)) x;
  RS.Rule.make "I_1" (m (i x) (m x y)) y;
  RS.Rule.make "I_2" (m x (m (i x) y)) y;
  RS.Rule.make "H" (i (m x y)) (m (i y) (i x))
]

let () =
  Printf.printf "%s\n\n%!" (RS.to_string groups)

(* let () = *)
  (* let r = RS.Rule.make "r1" (m e x) x in *)
  (* let r' = RS.Rule.refresh r in *)
(* assert (RS.Rule.eq r r') *)

let rule_name = Utils.namer (fun (s1,s2) (s1',s2') -> RS.Path.eq s1 s1' && RS.Path.eq s2 s2')
(* TODO: equality does not seem to be working... *)
(* let rule_name = Utils.namer (=) *)

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

let rule_name = Utils.namer (fun (s1,s2) (s1',s2') -> RS.Zigzag.eq s1 s1' && RS.Zigzag.eq s2 s2')
(* let rule_name = Utils.namer (=) *)

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
  let cpres =
    let coherence = List.mapi (fun i (p1,p2) -> "C"^string_of_int (i+1), (p1, p2)) coherence in
    RS.Coherent.make ops groups coherence
  in
  (* RS.Coherent.view_pdf cpres; *)
  let cpres = RS.Coherent.elim_rule cpres "E" "C12" in
  (* let cpres = RS.Coherent.elim_rule cpres "E_r" "C36" in *)
  (* let cpres = RS.Coherent.elim_rule cpres "I_r" "C16" in *)
  let cpres = RS.Coherent.elim_rule cpres "I_i" "C28" in
  let cpres = RS.Coherent.elim_rule cpres "I_1" "C5" in
  let cpres = RS.Coherent.elim_rule cpres "I_2" "C7" in
  Printf.printf "================ eliminated:\n%s\n%!" (RS.Coherent.to_string ~var:Var.namer_natural cpres);
  RS.Coherent.view_pdf cpres

(* for Im:
   add K36 : i(m(x62,x63)) -> m(i(x63),i(x62))
   m(i(m(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))),i(m(i(m(x61,i(x62))),x61))) -K35(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))-> i(m(x62,x63))
   m(i(m(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))),i(m(i(m(x61,i(x62))),x61))) -m(i(K7(x61,x62,x63)),i(m(i(m(x61,i(x62))),x61)))-> m(i(x63),i(m(i(m(x61,i(x62))),x61))) -m(i(x63),i(K35(x61,i(x62))))-> m(i(x63),i(i(i(x62)))) -m(i(x63),i(K23(x62)))-> m(i(x63),i(x62))
*)
