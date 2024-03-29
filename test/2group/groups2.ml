(* Testing Squier completion *)

open Alg
open Extlib
open Term

let ts_m a = Printf.sprintf "m(%s,%s)" (List.nth a 0) (List.nth a 1)
let ts_e _ = "e()"
let ts_i a = Printf.sprintf "i(%s)" (List.hd a)
let ts_j a = Printf.sprintf "j(%s)" (List.hd a)

(* let ts_m a = Printf.sprintf "(%s%s)" (List.nth a 0) (List.nth a 1) *)
(* let ts_e a = "1" *)
(* let ts_i a = Printf.sprintf "\\overline{%s}" (List.hd a) *)
(* let ts_j a = Printf.sprintf "\\underline{%s}" (List.hd a) *)

let m = Op.make ~to_string:ts_m "m" 2
let e = Op.make ~to_string:ts_e "e" 0
let i = Op.make ~to_string:ts_i "i" 1
let j = Op.make ~to_string:ts_j "j" 1
let ops = [m; e; i]
let e = app e []
let x = var ()
let y = var ()
let z = var ()

let m x y = app m [x;y]
let i x = app i [x]
let j x = app j [x]
let groups =
  RS.make ops
    [
      RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
      RS.Rule.make "L" (m e x) x;
      RS.Rule.make "R" (m x e) x;
      RS.Rule.make "I" (m (i x) x) e;
      RS.Rule.make "J" (m x (j x)) e;
      RS.Rule.make "E"  (i e) e;
      RS.Rule.make "F"  (j e) e;
      RS.Rule.make "N" (i (i x)) x;
      RS.Rule.make "O" (j (j x)) x;
      RS.Rule.make "T" (m (i x) (m x y)) y;
      RS.Rule.make "U" (m x (m (j x) y)) y;
      RS.Rule.make "G" (i (m x y)) (m (i y) (i x));
      RS.Rule.make "H" (j (m x y)) (m (j y) (j x))
    ]

let () =
  Printf.printf "presentation:\n%s\n\n%!" (RS.to_string groups)

(* let () = *)
  (* let groups = RS.knuth_bendix groups in *)
  (* Printf.printf "completion:\n%s\n\n%!" (RS.to_string groups); *)

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

(*
let rule_name = Utils.namer RS.Zigzag.eq
(* let rule_name = Utils.namer (=) *)

let coherence = List.map (fun (p1,p2) -> RS.Loop.of_cell (RS.Zigzag.of_path p1) (RS.Zigzag.of_path p2)) coherence

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
    let coherence = List.mapi (fun i p -> RS.Coherence.make ("C"^string_of_int (i+1)) p) coherence in
    RS.Coherent.make groups coherence
  in
  (*
  let cpres = RS.Coherent.elim_rule cpres "E" "C12" in
  let cpres = RS.Coherent.elim_rule cpres "N" "C30" in
  let cpres = RS.Coherent.elim_rule cpres "T" "C5" in
  let cpres = RS.Coherent.elim_rule cpres "U" "C7" in
  let cpres = RS.Coherent.elim_rule cpres "H" "CH" in
  Printf.printf "================ eliminated:\n%s\n%!" (RS.Coherent.to_string ~var:Var.namer_natural cpres);
  let rotations = ["C6", 7; "C8", 1; "C9", 8; "C10", 1; "C17", -2; "C18", 9; "C19", 9; "C21", -2; "C22", 9; "C23", 1; "C25", -12; "C26", -7; "C32", -2; "C33", -2; "C35", -7; "C36", -7; "C38", 1; "C40", 1; "C41", 3; "C42", -2; "C44", 3; "C45", -7; "C47", 6; "C48", 5; "C49", 6; "C50", 6; "C51", 13; "C52", 10] in
  let cpres = List.fold_left (fun cpres (c,n) -> RS.Coherent.rotate cpres c n) cpres rotations in
  *)
  RS.Coherent.view_pdf cpres

(* for Im:
   add K36 : i(m(x62,x63)) -> m(i(x63),i(x62))
   m(i(m(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))),i(m(i(m(x61,i(x62))),x61))) -K35(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))-> i(m(x62,x63))
   m(i(m(i(m(i(m(x61,i(x62))),x61)),m(x62,x63))),i(m(i(m(x61,i(x62))),x61))) -m(i(K7(x61,x62,x63)),i(m(i(m(x61,i(x62))),x61)))-> m(i(x63),i(m(i(m(x61,i(x62))),x61))) -m(i(x63),i(K35(x61,i(x62))))-> m(i(x63),i(i(i(x62)))) -m(i(x63),i(K23(x62)))-> m(i(x63),i(x62))
*)
*)
