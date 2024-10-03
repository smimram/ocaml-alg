(** Try generating terms. *)

open Alg
open Term

let m = Op.make "m" 2
let ops = [m]

let () =
  List.iter
    (fun t ->
       print_endline (Term.to_string t)
    ) (Term.generate_ops ops 3)

(*
let () =
  let x = var () in
  let y = var () in
  let m x y = app m [x;y] in
  let s = Renaming.unify_opt [] (m x y) (m x x) in
  Printf.printf "unifiable: %b\n%!" (s <> None)
*)
