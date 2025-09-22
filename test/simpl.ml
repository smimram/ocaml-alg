(** Testing the simplicial category. *)

open Alg

module S = Category.Simplicial

let () =
  let n = 8 in
  let k = 5 in
  (* Source and targets. *)
  assert (S.src (S.id n) = n);
  assert (S.tgt (S.id n) = n);
  assert (S.src (S.degeneracy n k) = n + 2);
  assert (S.tgt (S.degeneracy n k) = n + 1);
  assert (S.src (S.face n k) = n);
  assert (S.tgt (S.face n k) = n + 1);
  (* Simplicial relations. *)
  for i = 0 to n + 1 do
    for j = 0 to n do
      (* Printf.printf "testing s%d/s%d\n%!" i j; *)
      assert (
        S.E.eq
          (S.comp (S.degeneracy (n+1) i) (S.degeneracy n j))
          (
            if i <= j
            then S.comp (S.degeneracy (n+1) (j+1)) (S.degeneracy n i)
            else S.comp (S.degeneracy (n+1) j) (S.degeneracy n (i-1))
          )
      ) 
    done
  done;
  for i = 0 to n do
    for j = 0 to n+1 do
      (* Printf.printf "testing d%d/d%d\n%!" i j; *)
      assert (
        S.E.eq
          (S.comp (S.face n i) (S.face (n+1) j))
          (
            if i < j
            then S.comp (S.face n (j-1)) (S.face (n+1) i)
            else S.comp (S.face n j) (S.face (n+1) (i+1))
          )
      )
    done
  done;
  for i = 0 to n+1 do
    for j = 0 to n do
      (* Printf.printf "testing d%d/s%d\n%!" i j; *)
      assert (
        S.E.eq
          (S.comp (S.face (n+1) i) (S.degeneracy n j))
          (
            if i < j
            then S.comp (S.degeneracy (n-1) (j-1)) (S.face n i)
            else if i = j || i = j+1 then S.id (n+1)
            else S.comp (S.degeneracy (n-1) j) (S.face n (i-1))
          )
      )
    done
  done;
  (* Application. *)
  let f = S.degeneracy n k in
  for i = 0 to n do
    assert (
      S.ap f i =
      if i <= k then i else i-1
    )
  done
