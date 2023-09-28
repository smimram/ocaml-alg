open Alg

module M = struct
  include Monoid.Free(Alphabet.Char)

  let s s : t =
    Array.init (String.length s) (fun i -> s.[i])
end
module K = Field.Int
module P = Algebra.Pres(K)(Alphabet.Char)
module A = P.A

let () =
  let alphabet = ['a'; 'b'; 'c'; 'd'; 'e'; 'g'; 'f'] in
  let relations = [
    A.add
      (A.inj (M.s "aab"))
      (A.neg (A.inj (M.s "acc")))
    ;
    A.add
      (A.inj (M.s "ccd"))
      (A.neg (A.inj (M.s "f")))
    ;
    A.add
      (A.inj (M.s "cce"))
      (A.neg (A.inj (M.s "g")))
    ;
    A.add
      (A.inj (M.s "cg"))
      (A.neg (A.inj (M.s "fe")))
  ] in
  let pres = P.make (M.Order.deglex Alphabet.Char.geq) alphabet relations in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  let augmentation = P.Augmentation.monoid pres in
  let pres = P.buchberger pres in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  let d,s = P.Anick.resolution_ch ~augmentation pres 4 in
  Printf.printf "%s\n%!" (P.Anick.AMod.Pres.Complex.to_string d);
  let x = P.Anick.AMod.cinj (M.Anick.singleton 'a') (A.inj (M.s "bcd")) in
  Printf.printf "s(%s) = %s\n%!" (P.Anick.AMod.to_string x) (P.Anick.AMod.to_string (s.(1) x))
