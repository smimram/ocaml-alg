(** Testing parity complexes. *)

open Alg

let () =
  (* The example of a pasting diagram with whiskers. *)
  let module P = Parity.Pres(Alphabet.String) in
  let chain l = P.Set.of_list l in
  let s =
    List.fold_left
      (fun s (g,x,y) -> P.add s g (chain x, chain y))
      (List.fold_left P.add_point P.empty ["A";"B";"C";"D"])
      [
        "f",["A"],["C"];
        "g1",["A"],["B"];
        "g2",["B"],["C"];
        "g3",["C"],["D"];
        "h",["C"],["D"];
        "p",["f"],["g1";"g2"];
        "q",["g3"],["h"];
      ]
  in
  let gen = P.Cell.generator s in
  let f, g1, g2, g3, h, p, q = gen "f", gen "g1", gen "g2", gen "g3", gen "h", gen "p", gen "q" in
  let g = P.Cell.comp g1 g2 in
  let print name c = Printf.printf "%s = %s\n%!" name (P.Cell.to_string s c) in
  print_string "# Whiskers\n\n";
  let pg3 = P.Cell.comp p g3 in
  print "p *0 g3" pg3;
  assert (P.Cell.dim pg3 = 2);
  assert (P.Cell.eq (P.Cell.src pg3) (P.Cell.comp f g3));
  assert (P.Cell.eq (P.Cell.tgt pg3) (P.Cell.comp g g3));
  assert (P.Cell.eq (P.Cell.isrc pg3 0) (gen "A"));
  assert (P.Cell.eq (P.Cell.itgt pg3 0) (gen "D"));
  (* The top-dimensional generators are not enough to tell apart a cell from its whiskering. *)
  assert (not (P.Cell.eq pg3 p));
  let fq = P.Cell.comp f q in
  let gq = P.Cell.comp g q in
  let ph = P.Cell.comp p h in
  print "f *0 q" fq;
  print "g *0 q" gq;
  print "p *0 h" ph;
  print "id f" (P.Cell.id f);
  (* Exchange law: composition is computed on tables, so that both sides are equal. *)
  let pq = P.Cell.comp ~k:0 p q in
  print "p *0 q" pq;
  print "(p *0 g3) *1 (g *0 q)" (P.Cell.comp pg3 gq);
  print "(f *0 q) *1 (p *0 h)" (P.Cell.comp fq ph);
  assert (P.Cell.eq pq (P.Cell.comp pg3 gq));
  assert (P.Cell.eq pq (P.Cell.comp fq ph));
  (* Identities are units. *)
  assert (P.Cell.eq p (P.Cell.comp p (P.Cell.id g)));
  assert (P.Cell.eq p (P.Cell.comp (P.Cell.id f) p));
  (* Non-composable cells are rejected. *)
  assert (match P.Cell.comp g3 p with _ -> false | exception Invalid_argument _ -> true)

let () =
  for n = 0 to 4 do
    Printf.printf "# %d-globe\n\n%!" n;
    let c = Parity.Globe.make n in
    print_string @@ Parity.Globe.to_string c;
    print_newline ();
    print_newline ()
  done;
  for n = 0 to 4 do
    Printf.printf "# %d-cube\n\n%!" n;
    let c = Parity.Cube.make n in
    print_string @@ Parity.Cube.to_string c;
    print_newline ();
    print_newline ()
  done;
  for n = 0 to 4 do
    Printf.printf "# %d-oriental\n\n%!" n;
    let c = Parity.Oriental.make n in
    print_string @@ Parity.Oriental.to_string c;
    print_newline ();
    print_newline ()
  done
