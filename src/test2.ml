module M = struct
  include Monoid.Free(Alphabet.Char)

  let s s : t =
    Array.init (String.length s) (fun i -> s.[i])
end
module K = Field.Int
module P = Algebra.Pres(K)(Alphabet.Char)
module A = P.A

let () =
  let alphabet = ['x'; 'y'] in
  let relations = [
    A.add
      (A.add (A.inj (M.s "xx")) (A.inj (M.s "xy")))
      (A.add (A.inj (M.s "yx")) (A.inj (M.s "yy")))
  ] in
  (*
  let relations = [
    A.add (A.inj (M.s "xx"))
      (A.neg
         (A.add
            (A.inj (M.s "xy"))
            (A.add (A.inj (M.s "yx")) (A.inj (M.s "yy"))))
      )
  ] in
  *)
  (* let relations = [ *)
    (* A.sub (A.inj (M.s "xx")) (A.inj (M.s "yy")) *)
  (* ] in *)
  let relations = [
    A.sub (A.inj (M.s "xx")) A.one
  ]
  in
  let pres = P.make (M.Order.deglex Alphabet.Char.geq) alphabet relations in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  let augmentation = P.Augmentation.monoid pres in
  let pres = P.buchberger pres in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  let n = 10 in
  let d = P.Anick.resolution ~augmentation pres (n+1) in
  Printf.printf "%s\n%!" (P.Anick.AMod.Pres.Complex.to_string d);
  let d = P.Anick.complex ~augmentation pres (n+1) in
  Printf.printf "%s\n%!" (P.Anick.KMod.Pres.Complex.to_string d);
  assert (P.Anick.KMod.Pres.Complex.valid d);
  let h = P.Anick.homology ~augmentation pres n in
  Array.iteri (fun i n -> Printf.printf "H%d = %d\n" i n) h;
  ()
