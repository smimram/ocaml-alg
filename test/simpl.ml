(** Testing the simplicial category. *)

open Alg

module S = Category.Simplicial

let () =
  let n = 12 in
  assert (S.src (S.id n) = n);
  assert (S.tgt (S.id n) = n);
  ()
