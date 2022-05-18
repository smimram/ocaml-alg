open Alg

module X = struct
  include Alphabet.Int
  let to_string n = String.make 1 (char_of_int (int_of_char 'a' + n))
end
module M = Monoid.Free(X)
module K = Field.Int
module P = Algebra.Pres(K)(X)
module Gen = Algebra.Generate(K)(X)

let () =
  let leq = M.Order.deglex X.leq in
  (* let pres = Gen.symmetric leq 6 in *)
  let pres = Gen.exterior leq 3 in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  let pres = P.buchberger pres in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  let n = 10 in
  (* let augmentation = P.Augmentation.monoid pres in *)
  let augmentation = P.Augmentation.graded pres in
  let d = P.Anick.resolution ~augmentation pres n in
  Printf.printf "%s\n%!" (P.Anick.AMod.Pres.Complex.to_string d);
  let d = P.Anick.complex ~augmentation pres (n+1) in
  Printf.printf "%s\n%!" (P.Anick.KMod.Pres.Complex.to_string d);
  assert (P.Anick.KMod.Pres.Complex.valid d);
  let h = P.Anick.betti ~augmentation pres (n-1) in
  Array.iteri (fun i n -> Printf.printf "H%d = %d\n" i n) h
