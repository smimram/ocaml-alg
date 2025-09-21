(** Testing the simplicial category. *)

open Alg

module S = Category.Simplicial

let () =
  let n = 12 in
  let i = 5 in
  assert (S.src (S.id n) = n);
  assert (S.tgt (S.id n) = n);
  assert (S.src (S.degeneracy n i) = n + 2);
  assert (S.tgt (S.degeneracy n i) = n + 1);
  assert (S.src (S.face n i) = n);
  assert (S.tgt (S.face n i) = n + 1);
  ()
