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
  done;
  (* From fun. *)
  let n = 3 in
  let k = 1 in
  let f i = if i <= k then i else i-1 in
  let f = S.from_fun (n+2) (n+1) f in
  (* Printf.printf "d%d/%d = %s\n%!" k n (S.E.to_string f); *)
  assert (S.src f = (n+2));
  assert (S.tgt f = (n+1));
  assert (S.E.eq f (S.degeneracy n k));
  (* Test interval map. *)
  let f = function 0 | 1 -> 1 | 2 -> 3 | 3 -> 4 | _ -> assert false in
  let f = S.from_fun 4 5 f in
  Printf.printf "f = %s\n%!" (S.E.to_string f);
  let g = S.interval f in
  Printf.printf "g = %s\n%!" (S.E.to_string g);
  let g' = S.from_fun 6 5 (function 0 | 1 -> 0 | 2 | 3 -> 2 | 4 -> 3 | 5 -> 4 | _ -> assert false) in
  Printf.printf "g' = %s\n%!" (S.E.to_string g');
  assert (S.E.eq g g');
  
