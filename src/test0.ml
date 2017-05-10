module M = struct
  include Monoid.Free(Alphabet.Char)

  let s s : t =
    Array.init (String.length s) (fun i -> s.[i])
end
module K = Field.Int
module P = Algebra.Presentation(K)(Alphabet.Char)
module A = P.A

let () =
  Printexc.record_backtrace true;
  let alphabet = ['a'; 'b' (*; 'c'; 'd'; 'e'*)] in
  let l = [M.s "aaa"] in
  let cc = M.Anick.singletons alphabet in
  let cc = ref cc in
  for i = 0 to 6 do
    Printf.printf "%s\n%!" (String.concat " " (List.map M.Anick.to_string !cc));
    cc := M.Anick.extend l !cc
  done
