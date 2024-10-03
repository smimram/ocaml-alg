(* Example from
   https://terrytao.wordpress.com/2024/09/25/a-pilot-project-in-universal-algebra-to-explore-new-ways-to-collaborate-and-use-machine-assistance/
*)

open Alg
open Term

let m = Op.make "m" 2
let ops = [m]
let nops = 1
let r =
  List.init (nops+1)
    (fun i ->
       let l = Term.generate_ops ops i in
       List.map (fun t ->
           let vars = Term.vars t in
           List.map (fun u -> t, u) (Term.generate_ops ~vars ops (nops - i))
         ) l |> List.flatten
    ) |> List.flatten

let () =
  List.iter
    (fun (t,u) ->
       let var = Var.namer_natural () in
       Printf.printf "%s = %s\n%!" (Term.to_string ~var t) (Term.to_string ~var u)
    ) r

let r =
  let convertible (t1,t2) (u1,u2) =
    match Renaming.unify_opt [] t1 u1 with
    | None -> false
    | Some s -> Renaming.unify_opt s t2 u2 <> None
  in
  let op (t1,t2) = (t2,t1) in
  let rec filter = function
    | r::l ->
      let l = List.filter (fun r' -> not (convertible r r') && not (convertible (op r) r')) l in
      r::(filter l)
    | [] -> []
  in
  filter r

let () =
  print_endline "\n# Filtered\n";
  List.iter
    (fun (t,u) ->
       let var = Var.namer_natural () in
       Printf.printf "%s = %s\n%!" (Term.to_string ~var t) (Term.to_string ~var u)
    ) r

(*

let m x y = app m [x;y]

let x = var ()
let y = var ()

let rs =
  RS.make ops
    [
      RS.Rule.make "R" (m (m x x) y) (m y x);
    ]

let var = Var.namer_natural

let () = Printf.printf "# Theory\n\n%s\n\n%!" (RS.to_string ~var rs)

let gt = LPO.gt (>=)

let rs = RS.orient ~gt rs

let () = Printf.printf "# Oriented\n\n%s\n\n%!" (RS.to_string ~var rs)

let rs =
  let n = ref 0 in
  let callback rs =
    incr n;
    if !n = 2 then
      (
        Printf.printf "%s\n\n%!" (RS.to_string ~var rs);
        exit 0
      )
  in
  RS.knuth_bendix ~gt ~callback rs

let () = Printf.printf "# Completion\n\n%s\n\n%!" (RS.to_string ~var:Var.namer_natural rs)

*)
