(* Testing Squier completion *)

open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let e = app e [||]
let i = Op.make "i" 1
let x = var ()
let y = var ()
let z = var ()

let m x y = app m [|x;y|]
let i x = app i [|x|]
let groups = [
  RS.Rule.make "A"  (m (m x y) z) (m x (m y z));
  RS.Rule.make "E_l" (m e x) x;
  RS.Rule.make "E_r" (m x e) x;
  RS.Rule.make "E"  (i e) e;
  RS.Rule.make "I_l" (m (i x) x) e;
  RS.Rule.make "I_r" (m x (i x)) e;
  RS.Rule.make "I_i" (i (i x)) x;
  RS.Rule.make "I_1" (m (i x) (m x y)) y;
  RS.Rule.make "I_2" (m x (m (i x) y)) y;
  RS.Rule.make "I_m" (i (m x y)) (m (i y) (i x))
]

let () =
  Printf.printf "%s\n\n%!" (RS.to_string ~var:Var.namer_natural groups)

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
       Printf.printf "%02d: %s\n    %s\n\n%!" n s1 s2
    ) coherence

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

let rules = groups

let () =
  Printf.printf "\n***** LaTeX *****\n\n%!";
  let oc = open_out "squierGroup.tex" in
  let print s = Printf.ksprintf (fun s -> (* print_string s; *) output_string oc s) s; in
  let namer = Term.Var.namer_natural in
  let rules =
    List.map
      (fun r ->
         let var = namer () in
         let s = Term.to_string ~var (RS.Rule.source r) in
         let t = Term.to_string ~var (RS.Rule.target r) in
         Printf.sprintf "%s &: %s \\to %s\\\\"
           (RS.Rule.name r) s t
      ) rules
  in
  let rules = String.concat "\n" rules in
  print "\\section{Rules}\n\n\\begin{align*}\n%s\n\\end{align*}\n\n" rules;
  print "\\section{Coherence}\n\n";
  List.iter
    (fun (p1,p2) ->
       let p1,p2 =
         if RS.Path.length p1 > RS.Path.length p2 then p2,p1 else p1,p2
       in
       let var = namer () in
       let st n p = "{" ^ RS.Step.label ~var (RS.Path.nth_step n p) ^ "}" in
       let tm n p = Term.to_string ~var (RS.Path.nth_term n p) in
       let cd =
         match RS.Path.length p1, RS.Path.length p2 with
         | 1, 1 ->
           Printf.sprintf "%s\\ar[d,bend right,\"%s\"']\\ar[d,bend left,\"%s\"]\\\\\n%s"
             (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p1)
         | 1, 2 ->
           Printf.sprintf "%s\\ar[dr,\"%s\"']\\ar[r,\"%s\"]&%s\\ar[d,\"%s\"]\\\\\n&%s"
             (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2)
         | 1, 3 ->
           Printf.sprintf "%s\\ar[drr,\"%s\"']\\ar[r,\"%s\"]&%s\\ar[r,\"%s\"]&%s\\ar[d,\"%s\"]\\\\\n&&%s"
             (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2)
         (*
         | 2, 1 ->
           Printf.sprintf "%s\\ar[d,\"%s\"']\\ar[dr,\"%s\"]\\\\\n%s\\ar[r,\"%s\"']&%s"
           (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p1) (st 1 p1) (tm 2 p1)
         *)
         | 2, 2 ->
           Printf.sprintf "%s\\ar[r,\"%s\"]\\ar[d,\"%s\"']&%s\\ar[d,\"%s\"]\\\\\n%s\\ar[r,\"%s\"']&%s"
             (tm 0 p1) (st 0 p2) (st 0 p1) (tm 1 p2) (st 1 p2) (tm 1 p1) (st 1 p1) (tm 2 p1)
         | l1, l2 -> Printf.sprintf "TODO: %d, %d" l1 l2
       in
       print "\\[\n\\begin{tikzcd}\n%s\n\\end{tikzcd}\n\\]\n\n" cd
    ) coherence;
  Printf.printf "Done.\n%!"
