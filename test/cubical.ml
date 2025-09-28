(** Testing the precubical category. *)

open Alg

module C = Category.Precubical

let () =
  let n = 8 in
  let k = 5 in
  let e = false in
  let e' = true in
  (* Source and targets. *)
  assert (C.src (C.id n) = n);
  assert (C.tgt (C.id n) = n);
  assert (C.src (C.face e n k) = n);
  assert (C.tgt (C.face e n k) = n + 1);
  (* Cubical relations. *)
  for i = 0 to n - 1 do
    for j = 0 to n do
      (* Printf.printf "testing s%d/s%d\n%!" i j; *)
      assert (
        C.E.eq
          (C.comp (C.face e n i) (C.face e' (n+1) j))
          (
            if i < j
            then C.comp (C.face e' n (j-1)) (C.face e (n+1) i)
            else C.comp (C.face e' n j) (C.face e (n+1) (i+1))
          )
      ) 
    done
  done
