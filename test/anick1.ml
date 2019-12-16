module M = struct
  include Monoid.Free(Alphabet.Char)

  let s s : t =
    Array.init (String.length s) (fun i -> s.[i])
end
module K = Field.Int
module P = Algebra.Pres(K)(Alphabet.Char)
module A = P.A

(*
let () =
  Printexc.record_backtrace true;
  let alphabet = ['a'; 'b' (*; 'c'; 'd'; 'e'*)] in
  (*
  let l = [M.s "ab"; M.s "bb"] in
  let cc = M.Anick.singletons alphabet in
  let cc = ref cc in
  for i = 0 to 5 do
    Printf.printf "%s\n%!" (String.concat_map " " M.Anick.to_string !cc);
    cc := M.Anick.extend l !cc
  done;
  *)
  let relations = [
    A.sub (A.inj (M.s "ab")) (A.inj (M.s "e"));
    A.sub (A.inj (M.s "bc")) (A.inj (M.s "d"));
    A.sub (A.inj (M.s "ad")) (A.inj (M.s "ec"))
  ] in
  let relations = [ A.sub (A.inj (M.s "aa")) (A.inj (M.s "a")) ] in
  let relations = [
    A.sub (A.inj (M.s "aa")) (A.inj (M.s "a"));
    A.sub (A.inj (M.s "bb")) (A.inj (M.s "b"));
    A.sub (A.inj (M.s "aba")) (A.inj (M.s "bab"));
  ]
  in
  let pres = P.make (M.Order.deglex Alphabet.Char.leq) alphabet relations in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  let n = 2 in
  let d = P.Anick.resolution pres n in
  Printf.printf "%s\n%!" (P.Anick.AMod.Pres.Complex.to_string d);
  let h = P.Anick.homology pres n in
  Array.iteri (fun i n -> Printf.printf "H%d = %d\n" i n) h;
  ()
*)

let () =
  (* let alphabet = ['a'; 'b' ; 'c'; 'd'; 'e'] in *)
  (*
  let relations = [
    A.sub (A.inj (M.s "ab")) (A.inj (M.s "e"));
    A.sub (A.inj (M.s "bc")) (A.inj (M.s "d"));
    (* A.sub (A.inj (M.s "ad")) (A.inj (M.s "ec")) *)
  ] in
  *)
  (* let relations = [ *)
    (* A.sub (A.inj (M.s "ab")) (A.inj (M.s "ee")); *)
    (* A.sub (A.inj (M.s "bc")) (A.inj (M.s "ed")); *)
  (* ] in *)
  let alphabet = ['x'; 'y'; 'z'] in
  let relations = [
    A.sub
      (A.add (A.add (A.inj (M.s "xxx")) (A.inj (M.s "yyy"))) (A.inj (M.s "zzz")))
      (A.inj (M.s "xyz"))
  ] in
  let pres = P.make (M.Order.deglex Alphabet.Char.leq) alphabet relations in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  Printf.printf "Completing...\n%!";
  let pres = P.buchberger pres in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  Printf.printf "Reducing...\n%!";
  let pres = P.reduce pres in
  Printf.printf "%s\n\n%!" (P.to_string pres);
  Printf.printf "Resolving...\n%!";
  let n = 5 in
  let d = P.Anick.resolution pres n in
  Printf.printf "%s\n%!" (P.Anick.AMod.Pres.Complex.to_string d);
  Printf.printf "Building complex...\n%!";
  let d = P.Anick.complex pres n in
  assert (P.Anick.KMod.Pres.Complex.valid d);
  Printf.printf "%s\n%!" (P.Anick.KMod.Pres.Complex.to_string d);
  Printf.printf "Computing homology...\n%!";
  let h = P.Anick.betti pres n in
  Array.iteri (fun i n -> Printf.printf "H%d = %d\n" i n) h
