module A = Automaton.Make(Alphabet.Char)
module R = A.Regexp
module S = A.Regexp.Series

let () =
  (* bba as factor *)
  let trans = [
      0, 'a', 0;
      0, 'b', 1;
      1, 'a', 0;
      1, 'b', 2;
      2, 'a', 3;
      2, 'b', 2;
      3, 'a', 3;
      3, 'b', 3
    ]
  in
  let aut = A.create 4 0 [3] trans in
  (*
  (* bb as factor *)
  let trans = [
      0, 'a', 0;
      0, 'b', 1;
      1, 'a', 0;
      1, 'b', 2;
      2, 'a', 2;
      2, 'b', 2;
    ]
  in
  let aut = A.create 3 0 [2] trans in
  *)
  (*
  (* a as factor *)
  let trans = [
      0, 'a', 1;
      1, 'b', 0;
      1, 'a', 1;
      1, 'b', 1;
    ]
  in
  let aut = A.create 2 0 [1] trans in
  *)
  let r = A.kleene aut in
  let r = A.Regexp.simpl r in
  Printf.printf "%s\n%!" (A.Regexp.to_string r);
  let g = (A.Regexp.series r) in
  Printf.printf "%s\n%!" (A.Regexp.Series.to_string g)
