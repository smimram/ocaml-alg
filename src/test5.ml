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
    RS.Rule.make "r1" (m e x) x;
    RS.Rule.make "r2" (m (i x) x) e;
    RS.Rule.make "r3" (m (m x y) z) (m x (m y z));
    RS.Rule.make "r4" (m (i x) (m x y)) y;
    RS.Rule.make "r8" (m x e) x;
    RS.Rule.make "r9" (i e) e;
    RS.Rule.make "r10" (i (i x)) x;
    RS.Rule.make "r11" (m x (i x)) e;
    RS.Rule.make "r13" (m x (m (i x) y)) y;
    RS.Rule.make "r20" (i (m x y)) (m (i y) (i x))
  ]

let () =
  Printf.printf "%s\n\n%!" (RS.to_string groups)

(* let () = *)
  (* let r = RS.Rule.make "r1" (m e x) x in *)
  (* let r' = RS.Rule.refresh r in *)
  (* assert (RS.Rule.eq r r') *)

let () =
  List.iter
    (fun (s1,s2) ->
      Printf.printf "%s\n%s\n\n%!" (RS.Path.to_string s1) (RS.Path.to_string s2)
    ) (RS.squier groups)
