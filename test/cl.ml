(** Test combinatory logic. *)

open Alg
open CombinatoryLogic

let () =
  print_endline (to_string (App (App (S, K), I)));
  print_endline (to_string (App (S, App (K, I))));
  let test s = s |> of_string |> to_string |> print_endline in
  test "S K I";
  test "(S K) I";
  test "S (K I)";
  let normalize s = s |> of_string |> normalize |> to_string |> print_endline in
  normalize "S I I I";
  normalize "S(S(K S) I)(K I)"
