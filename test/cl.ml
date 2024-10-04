(** Test combinatory logic. *)

open Alg
open CombinatoryLogic

let () =
  print_endline (to_string (App (App (S, K), I)));
  print_endline (to_string (App (S, App (K, I))));
  let test s = print_endline (to_string (of_string s)) in
  test "S K I";
  test "(S K) I";
  test "S (K I)"
