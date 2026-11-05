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
  assert (match P.Cell.comp g3 p with _ -> false | exception Invalid_argument _ -> true);
  (* Ill-formed generators are rejected. *)
  let rejected x y = match P.add s "r" (chain x, chain y) with _ -> false | exception Invalid_argument _ -> true in
  assert (rejected ["f";"g3"] ["f";"h"]);
  assert (rejected ["f"] ["g3"]);
  assert (rejected ["g3"] ["f"]);
  assert (not (rejected ["f"] ["g1";"g2"]));
  (* Well-formed chains. *)
  let well_formed n l = P.Chain.well_formed s n (chain l) in
  assert (well_formed 0 ["A"]);
  assert (not (well_formed 0 ["A";"B"]));
  assert (not (well_formed 0 []));
  assert (well_formed 1 []);
  assert (well_formed 1 ["g1";"g2"]);
  assert (well_formed 1 ["g1";"g3"]);
  assert (not (well_formed 1 ["f";"g1"]));
  assert (not (well_formed 1 ["g3";"h"]));
  assert (not (well_formed 1 ["A";"f"]));
  assert (well_formed 2 ["p";"q"]);
  (* Faces which are not well-formed in some dimension are rejected. *)
  assert (rejected ["A";"B"] ["C"]);
  assert (rejected ["C"] ["A";"B"]);
  assert (rejected ["g3";"h"] ["f"]);
  (* The following faces are parallel, disjoint and well-formed in dimension 1, but they are not composable because their iterated source is not a point. *)
  let s' = P.add s "a" (chain ["A"], chain ["B"]) in
  assert (match P.add s' "r" (chain ["g1";"g3"], chain ["a";"h"]) with _ -> false | exception Invalid_argument _ -> true)

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

let () =
    let module P = Parity.PastingScheme in
  let module PS = PastingScheme in
  let pt = PS.point in
  let ps l = PS.PS l in
  print_string "# Pasting schemes\n\n";
  List.iter
    (fun p ->
       let s = P.make p in
       Printf.printf "## %s\n\n%s\n\n%s\n\n%!" (PS.to_string p) (P.to_string s) (P.Cell.to_string s (P.cell s));
       assert (P.max_dim s = PS.dim p);
       (* There is one generator for each sector of each node of the tree. *)
       let rec sectors (PS.PS l) = List.fold_left (fun n p -> n + sectors p) (1 + List.length l) l in
       assert (List.length (P.to_list s) = sectors p)
    )
    [
      pt;
      ps [pt];
      ps [pt; pt];
      ps [ps [pt]];
      ps [ps [pt; pt]];
      ps [ps [pt]; ps [pt]];
      ps [pt; ps [pt]];
      ps [ps [ps [pt]]];
      ps [ps [pt; pt]; pt; ps [ps [pt]; pt]];
    ];
  (* The whole cell keeps track of whiskers. *)
  let s = P.make (ps [pt; ps [pt]]) in
  let u, v = P.Cell.src (P.cell s), P.Cell.tgt (P.cell s) in
  assert (P.Cell.eq u (P.Set.of_list [1,0; 1,1], [P.Set.of_list [0,0], P.Set.of_list [0,2]]));
  assert (P.Cell.eq v (P.Set.of_list [1,0; 1,2], [P.Set.of_list [0,0], P.Set.of_list [0,2]]));
  (* The point is a unit for wedge. *)
  let eq = P.Map.equal (fun (n,u,v) (n',u',v') -> n = n' && P.Set.equal u u' && P.Set.equal v v') in
  assert (eq (P.wedge P.point s) s);
  assert (eq (P.wedge s P.point) s);
  (* Wedge is associative. *)
  let a, b, c = P.make (ps [pt]), P.make (ps [ps [pt]]), P.make (ps [pt; ps [pt; pt]]) in
  assert (eq (P.wedge (P.wedge a b) c) (P.wedge a (P.wedge b c)));
  (* Only endpoints can be identified. *)
  let swapped = P.add_point (P.add_point P.empty (0,0)) (0,1) in
  assert (match P.wedge swapped a with _ -> false | exception Invalid_argument _ -> true)

(* Well-formedness of complexes. *)
let () =
  for n = 0 to 4 do
    assert (Parity.Globe.check (Parity.Globe.make n));
    assert (Parity.Cube.check (Parity.Cube.make n));
    assert (Parity.Oriental.check (Parity.Oriental.make n))
  done;
  let module PS = PastingScheme in
  let pt = PS.point in
  let ps l = PS.PS l in
  List.iter
    (fun p -> assert (Parity.PastingScheme.(check (make p))))
    [pt; ps [pt; pt]; ps [ps [pt]; ps [pt]]; ps [ps [pt; pt]; pt; ps [ps [pt]; pt]]];
  let module P = Parity.Pres(Alphabet.String) in
  let chain l = P.Set.of_list l in
  let complex points l =
    List.fold_left
      (fun s (g,x,y) -> P.add s g (chain x, chain y))
      (List.fold_left P.add_point P.empty points)
      l
  in
  assert (P.check P.empty);
  let edges = ["f",["A"],["B"]; "g",["A"],["B"]; "h",["A"],["B"]] in
  assert (P.check (complex ["A";"B"] edges));
  (* A loop is accepted by add but is not well-formed. *)
  assert (not (P.check (complex ["A";"B"] ["f",["A"],["B"]; "g",["B"],["A"]])));
  (* Here x ◁ y' even though x is in the source of z and y' in its target. *)
  let squares = ["x",["f"],["g"]; "x'",["g"],["h"]; "y",["f"],["g"]; "y'",["g"],["h"]] in
  assert (P.check (complex ["A";"B"] (edges @ squares)));
  assert (not (P.check (complex ["A";"B"] (edges @ squares @ ["z",["x";"x'"],["y";"y'"]]))));
  (* Complexes which are not built using add. *)
  let s = complex ["A";"B"] edges in
  assert (not (P.check (P.Map.add "r" (1, chain ["C"], chain ["B"]) s)));
  assert (not (P.check (P.Map.add "r" (1, chain ["A"], chain ["A"]) s)));
  assert (not (P.check (P.Map.add "r" (1, chain [], chain []) s)));
  assert (not (P.check (P.Map.add "r" (0, chain ["A"], chain ["B"]) s)));
  assert (not (P.check (P.Map.add "r" (2, chain ["A"], chain ["B"]) s)))

let () =
  let module C = Parity.CubeMorphism in
  print_string "# Degeneracies and connections\n\n";
  let max_n = 4 in
  let cube = Array.init (max_n + 1) C.make in
  let bools = [false; true] in
  let image n n' f = C.Hom.cell (C.Hom.of_partial cube.(n) cube.(n') f) in
  (* A map from the n-cube to the n'-cube preserves generators and their boundaries, up to identities. *)
  let check n n' f =
    assert (C.Hom.check (C.Hom.of_partial cube.(n) cube.(n') f));
    List.iter
      (fun g ->
         let c = C.Cell.generator cube.(n) g in
         let expected =
           match f g with
           | Some g' -> C.Cell.generator cube.(n') g'
           | None -> C.Cell.id (image n n' f (C.Cell.src c))
         in
         assert (C.Cell.eq (image n n' f c) expected)
      ) (C.to_list cube.(n))
  in
  for n = 1 to max_n do
    for i = 0 to n - 1 do
      check n (n-1) (C.deg i);
      List.iter (fun e -> check (n-1) n (C.face e i)) bools
    done;
    for i = 0 to n - 2 do
      List.iter (fun e -> check n (n-1) (C.con e i)) bools
    done
  done;
  (* Cubical identities, where f >> g means f followed by g. *)
  let (>>) f g x = Option.bind (f x) g in
  let same n f g = List.iter (fun x -> assert (f x = g x)) (C.to_list cube.(n)) in
  let id x = Some x in
  for n = 1 to max_n do
    for i = 0 to n - 1 do
      List.iter (fun e -> same (n-1) (C.face e i >> C.deg i) id) bools
    done;
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        same n (C.deg j >> C.deg i) (C.deg i >> C.deg (j-1))
      done;
      List.iter
        (fun e ->
           same n (C.con e i >> C.deg i) (C.deg i >> C.deg i);
           (* The value e is absorbing and the other one is neutral. *)
           List.iter
             (fun k ->
                same (n-1) (C.face e k >> C.con e i) (C.deg i >> C.face e i);
                same (n-1) (C.face (not e) k >> C.con e i) id
             ) [i; i+1]
        ) bools
    done;
    for i = 0 to n - 3 do
      List.iter (fun e -> same n (C.con e (i+1) >> C.con e i) (C.con e i >> C.con e i)) bools
    done
  done;
  (* Images are compatible with composition. *)
  let gen s l = C.Cell.generator s (Array.of_list l) in
  let bot, top, zero = Some false, Some true, None in
  let a = gen cube.(2) [bot; zero] in
  let b = gen cube.(2) [zero; top] in
  List.iter
    (fun e ->
       let f = image 2 1 (C.con e 0) in
       assert (C.Cell.eq (f (C.Cell.comp a b)) (C.Cell.comp (f a) (f b)))
    ) bools;
  (* Connections on the square and on the 3-cube. *)
  let print n f g =
    let name = Array.of_list g |> C.A.to_string in
    Printf.printf "%s = %s\n%!" name (C.Cell.to_string cube.(n-1) (image n (n-1) f (gen cube.(n) g)))
  in
  List.iter
    (fun e ->
       Printf.printf "Connection %b:\n%!" e;
       print 2 (C.con e 0) [zero; zero];
       print 2 (C.con e 0) [zero; bot];
       print 3 (C.con e 0) [zero; zero; zero];
       print 3 (C.con e 1) [zero; zero; top]
    ) bools;
  (* A collapsed generator yields an identity, keeping the dimension. *)
  let c = image 2 1 (C.con true 0) (gen cube.(2) [zero; zero]) in
  assert (C.Cell.dim c = 2);
  assert (C.Cell.eq c (C.Cell.id (gen cube.(1) [zero])));
  (* Degeneracies reach the point. *)
  assert (C.deg 0 [|bot|] = Some C.A.one);
  assert (C.deg 0 [|zero|] = None);
  assert (C.Cell.dim (image 1 0 (C.deg 0) (gen cube.(1) [zero])) = 1)

let () =
  let module D = Parity.CubeMorphism.Box.D in
  let x, y, z = D.inj 0, D.inj 1, D.inj 2 in
  (* Absorption and bounds. *)
  assert (D.eq (D.sup x (D.inf x y)) x);
  assert (D.eq (D.inf x (D.sup x y)) x);
  assert (D.eq (D.sup x D.top) D.top);
  assert (D.eq (D.inf x D.bot) D.bot);
  assert (D.eq (D.sup x D.bot) x);
  assert (D.eq (D.inf x D.top) x);
  (* Distributivity, in both directions. *)
  assert (D.eq (D.inf x (D.sup y z)) (D.sup (D.inf x y) (D.inf x z)));
  assert (D.eq (D.sup x (D.inf y z)) (D.inf (D.sup x y) (D.sup x z)));
  (* Printing. *)
  assert (D.to_string D.bot = "⊥");
  assert (D.to_string D.top = "⊤");
  (* assert (D.to_string (D.sup (D.inf x y) z) = "x₀x₁∨x₂"); *)
  (* The free lattice on n generators has as many elements as there are monotone boolean functions on n variables (the Dedekind numbers), which would fail if the normal form was not canonical. *)
  let size n =
    let rec close l =
      let l' = List.sort_uniq D.compare (l @ List.concat_map (fun a -> List.concat_map (fun b -> [D.sup a b; D.inf a b]) l) l) in
      if List.compare_lengths l l' = 0 then l else close l'
    in
    List.length (close (List.sort_uniq D.compare (D.bot :: D.top :: List.init n D.inj)))
  in
  List.iteri (fun n c -> assert (size n = c)) [2; 3; 6; 20; 168]

let () =
  let module C = Parity.CubeMorphism in
  let module B = C.Box in
  let max_n = 4 in
  let cube = Array.init (max_n + 1) C.make in
  let bools = [false; true] in
  (* The action of a map on the generators of its domain, which determines it. *)
  let action k f = List.map f (C.to_list cube.(k)) in
  print_string "\n# Number of k-cells (columns) of the n-cube (rows)\n\n";
  let table =
    [
      [ 1;  1;   1;   1;    1];
      [ 2;  3;   6;  17;   64];
      [ 4;  8;  21;  71;  290];
      [ 8; 20;  62; 231;  990];
      [16; 48; 168; 672; 2993];
    ]
  in
  List.iteri
    (fun n l ->
       List.iteri
         (fun k c ->
            let c' = List.length (B.cells n k) in
            Printf.printf "%6d" c';
            assert (c = c')
         ) l;
       print_newline ()
    ) table;
  (* Compare with the closure under all operations on generators, allowing intermediate dimensions up to the given bound: this checks that the normal form is complete and that words and actions agree. *)
  let closure k bound =
    let seen = Hashtbl.create 1000 in
    let rec visit m a =
      if not (Hashtbl.mem seen (m,a)) then
        (
          Hashtbl.add seen (m,a) ();
          let apply f m' = visit m' (List.map (fun x -> Option.bind x f) a) in
          for i = 0 to m - 1 do apply (C.deg i) (m-1) done;
          for i = 0 to m - 2 do List.iter (fun e -> apply (C.con e i) (m-1)) bools done;
          if m < bound then
            for i = 0 to m do List.iter (fun e -> apply (C.face e i) (m+1)) bools done
        )
    in
    visit k (action k Option.some);
    seen
  in
  for k = 0 to 4 do
    let seen = closure k 5 in
    for n = 0 to 4 do
      let l = B.cells n k in
      let a = List.map (fun c -> action k (B.map c)) l in
      (* Distinct cells are distinct maps. *)
      assert (List.length (List.sort_uniq compare a) = List.length l);
      List.iter (fun a -> assert (Hashtbl.mem seen (n,a))) a;
      let closed = Hashtbl.fold (fun (m,_) () c -> if m = n then c + 1 else c) seen 0 in
      assert (closed = List.length l)
    done
  done;
  (* The structure maps of cells are in the listing and satisfy the cubical identities. *)
  let cells = Hashtbl.create 1000 in
  for n = 0 to max_n do
    for k = 0 to 5 do
      List.iter (fun c -> Hashtbl.replace cells (n,k,c) ()) (B.cells n k)
    done
  done;
  let mem n k c = Hashtbl.mem cells (n,k,c) in
  for n = 0 to max_n do
    for k = 0 to max_n do
      List.iter
        (fun c ->
           for i = 0 to k - 1 do
             List.iter (fun e -> assert (mem n (k-1) (B.face e i c))) bools
           done;
           for i = 0 to k do
             assert (mem n (k+1) (B.deg i c));
             List.iter (fun e -> assert (B.eq (B.face e i (B.deg i c)) c)) bools
           done;
           for i = 0 to k - 1 do
             List.iter
               (fun e ->
                  let d = B.con e i c in
                  assert (mem n (k+1) d);
                  assert (B.eq (B.con e i d) (B.con e (i+1) d));
                  (* The value e is absorbing and the other one is neutral. *)
                  List.iter
                    (fun j ->
                       assert (B.eq (B.face e j d) (B.deg i (B.face e i c)));
                       assert (B.eq (B.face (not e) j d) c)
                    ) [i; i+1]
               ) bools
           done
        ) (B.cells n k)
    done
  done;
  (* Composition is compatible with the action on generators and stays in the listing. *)
  for k = 0 to 3 do
    for m = 0 to 3 do
      for n = 0 to 3 do
        List.iter
          (fun f ->
             List.iter
               (fun g ->
                  let h = B.comp f g in
                  assert (mem n k h);
                  assert (action k (B.map h) = action k (fun x -> Option.bind (B.map f x) (B.map g)))
               ) (B.cells n m)
          ) (B.cells m k)
      done
    done
  done;
  (* The top cell of the k-cube is the identity. *)
  for k = 0 to max_n do
    assert (mem k k (B.id k));
    assert (action k (B.map (B.id k)) = action k Option.some)
  done;
  (* The four degenerate squares on the interval have the same top cell, and only differ by their boundary. *)
  let top = C.Cell.generator cube.(2) [|None;None|] in
  let squares = List.filter (fun c -> B.D.compare c.(0) B.D.bot <> 0 && B.D.compare c.(0) B.D.top <> 0) (B.cells 1 2) in
  assert (List.length squares = 4);
  List.iter (fun c -> assert (C.Cell.eq (C.Hom.cell (B.hom cube.(2) cube.(1) c) top) (C.Cell.id (C.Cell.generator cube.(1) [|None|])))) squares;
  (* Cells are morphisms, whose images do not merge generators, and composition of words is composition of morphisms. *)
  for n = 0 to max_n do
    for k = 0 to max_n do
      List.iter (fun c -> assert (C.Hom.check (B.hom cube.(k) cube.(n) c))) (B.cells n k)
    done
  done;
  for n = 0 to 3 do
    for k = 0 to 3 do
      List.iter
        (fun w ->
           let f = B.hom cube.(k) cube.(n) w in
           List.iter
             (fun g ->
                let c, b = C.Cell.generator cube.(k) g in
                assert (C.Hom.injective_on f c);
                List.iter (fun (u,v) -> assert (C.Hom.injective_on f u && C.Hom.injective_on f v)) b
             ) (C.to_list cube.(k))
        ) (B.cells n k)
    done
  done;
  for k = 0 to 3 do
    for m = 0 to 3 do
      for n = 0 to 3 do
        List.iter
          (fun f ->
             let f' = B.hom cube.(k) cube.(m) f in
             List.iter
               (fun g -> assert (C.Hom.eq (B.hom cube.(k) cube.(n) (B.comp f g)) (C.comp f' (B.hom cube.(m) cube.(n) g))))
               (B.cells n m)
          ) (B.cells m k)
      done
    done
  done;
  (* List the cells. *)
  for n = 0 to 2 do
    for k = 0 to 4 do
      Printf.printf "\n# %d-cells of the %d-cube\n\n" k n;
      List.iter (fun c -> print_endline (B.to_string c)) (B.cells n k)
    done
  done

(* Morphisms between arbitrary complexes. *)
let () =
  let module C = Parity.Cube in
  let module H = Parity.Morphism(C)(C) in
  (* Morphisms which should be rejected: reversing the orientation and not preserving dimensions. *)
  let i = C.make 1 in
  let swap = Array.map (Option.map not) in
  assert (not (H.check (H.of_partial i i (fun g -> Some (swap g)))));
  assert (not (H.check (H.make (C.make 2) i (fun _ -> C.Set.singleton [|None|]))));
  (* An edge sent to the whole boundary of a square: as sets its source and target are fine, but as a chain its boundary is twice what is expected. *)
  let vertex b = C.Set.singleton [|Some b; Some b|] in
  let sides = C.Set.of_list [[|Some false; None|]; [|Some true; None|]; [|None; Some false|]; [|None; Some true|]] in
  let around = H.make i (C.make 2) (function [|Some b|] -> vertex b | _ -> sides) in
  assert (not (H.check around));
  (* The n-globe maps to the n-cube, sending a generator to the corresponding boundary of the top cell: this is not a singleton in general and the alphabets are different. *)
  let module G = Parity.Globe in
  let module GC = Parity.Morphism(G)(C) in
  for n = 1 to 4 do
    let q = C.make n in
    let g = G.make n in
    let c, b = C.Cell.generator q (Array.make n None) in
    let b = Array.of_list (List.rev b) in
    let app (e,k) = if k = n then c else (if e then snd else fst) b.(k) in
    assert (GC.check (GC.make g q app));
    assert (n = 1 || List.exists (fun x -> C.Set.cardinal (app x) > 1) (G.to_list g));
    (* The top cell of the globe is the top cell of the cube. *)
    assert (C.Cell.eq (GC.cell (GC.make g q app) (G.Cell.generator g (true,n))) (c, Array.to_list b |> List.rev))
  done;
  (* Faces and degeneracies of orientals, which are induced by maps on vertices. *)
  let module O = Parity.Oriental in
  let module OO = Parity.Morphism(O)(O) in
  let map h l =
    let l' = List.map h l in
    if List.length (List.sort_uniq compare l') = List.length l then Some l' else None
  in
  for n = 1 to 4 do
    let o = O.make n in
    let o' = O.make (n-1) in
    for i = 0 to n - 1 do
      assert (OO.check (OO.of_partial o o' (map (fun x -> if x <= i then x else x-1))))
    done;
    for i = 0 to n do
      assert (OO.check (OO.of_partial o' o (map (fun x -> if x < i then x else x+1))))
    done
  done

(* Functors between free categories on cubes. *)
let () =
  let module C = Parity.CubeMorphism in
  let module H = C.Hom in
  let module O = C.Omega in
  let cube = Array.init 5 C.make in
  (* Number of functors, where the row of the interval consists of the Dedekind numbers (monotone boolean functions) and the column of k = 1 counts the directed paths in a cube along with its vertices. *)
  print_string "\n# Number of functors from the k-cube (columns) to the n-cube (rows)\n\n";
  List.iter
    (fun (n,l) ->
       List.iteri
         (fun k c ->
            let c' = List.length (O.cells n k) in
            Printf.printf "%6d" c';
            assert (c = c')
         ) l;
       print_newline ()
    ) [
    0, [ 1;   1;    1;   1;   1];
    1, [ 2;   3;    6;  20; 168];
    2, [ 4;  10;   47; 672];
    3, [ 8;  38;  506];
    4, [16; 168; 7846];
  ];
  (* The cells of the free categories, by dimension. *)
  let cells n d = Array.to_list (Array.map List.length (C.cells cube.(n) d)) in
  assert (cells 1 2 = [2; 3; 3]);
  assert (cells 2 3 = [4; 10; 11; 11]);
  assert (cells 3 3 = [8; 38; 56; 57]);
  (* All the functors are morphisms and they are distinct. *)
  let small = [0,0; 0,1; 0,2; 0,3; 1,0; 1,1; 1,2; 1,3; 2,0; 2,1; 2,2; 2,3; 3,0; 3,1; 3,2] in
  List.iter
    (fun (n,k) ->
       let l = O.cells n k in
       List.iter (fun f -> assert (H.check f)) l;
       let action f = List.map (fun g -> C.Set.elements (f.H.app g)) (C.to_list cube.(k)) in
       assert (List.length (List.sort_uniq compare (List.map action l)) = List.length l)
    ) small;
  (* The maps of the box category are functors. *)
  List.iter
    (fun (n,k) ->
       let l = O.cells n k in
       List.iter
         (fun w ->
            let f = C.Box.hom cube.(k) cube.(n) w in
            assert (List.exists (H.eq f) l)
         ) (C.Box.cells n k)
    ) small;
  (* Compare with the enumeration of positive chain maps: the theorem of Steiner states that these are the functors, and this does not use cells at all. Coefficients are integers since a chain of the cube which is not a cell can have generators with the same source. *)
  let chain_maps n k =
    let dom = cube.(k) in
    let cod = cube.(n) in
    let add a c m =
      C.Set.fold
        (fun g m ->
           let a = a + Option.value ~default:0 (C.Map.find_opt g m) in
           if a = 0 then C.Map.remove g m else C.Map.add g a m
        ) c m
    in
    let boundary c =
      C.Map.bindings (C.Set.fold (fun g m -> add 1 (C.tgt cod g) (add (-1) (C.src cod g) m)) c C.Map.empty)
    in
    let subsets l = List.fold_left (fun a x -> a @ List.map (C.Set.add x) a) [C.Set.empty] l in
    (* The chains in each dimension by boundary. *)
    let chains =
      Array.init (k+1)
        (fun d ->
           let t = Hashtbl.create 1000 in
           List.iter (fun c -> Hashtbl.add t (boundary c) c) (subsets (C.Set.elements (C.generators ~dim:d cod)));
           t)
    in
    let vertices = List.map C.Set.singleton (C.Set.elements (C.generators ~dim:0 cod)) in
    let rec aux m = function
      | [] -> 1
      | g::gens ->
        let d = C.dim dom g in
        let extend l = List.fold_left (fun total c -> total + aux (C.Map.add g c m) gens) 0 l in
        if d = 0 then extend vertices else
          let image c = C.Set.fold (fun x e -> C.Set.union (C.Map.find x m) e) c C.Set.empty in
          let injective c = C.Set.fold (fun x n -> n + C.Set.cardinal (C.Map.find x m)) c 0 = C.Set.cardinal (image c) in
          let s, t = C.src dom g, C.tgt dom g in
          if not (injective s && injective t) then 0 else
            let b = C.Map.bindings (add 1 (image t) (add (-1) (image s) C.Map.empty)) in
            extend (Hashtbl.find_all chains.(d) b)
    in
    aux C.Map.empty (C.to_list dom)
  in
  List.iter
    (fun (n,k) -> assert (chain_maps n k = List.length (O.cells n k)))
    [1,3; 2,1; 2,2; 2,3; 3,1; 3,2];
  (* The functors which are not in the box category: the two paths around a square... *)
  let is_box n k f = List.exists (fun w -> H.eq (C.Box.hom cube.(k) cube.(n) w) f) (C.Box.cells n k) in
  let paths = List.filter (fun f -> not (is_box 2 1 f)) (O.cells 2 1) in
  assert (List.length paths = 2);
  let edge = [|None|] in
  List.iter (fun f -> assert (C.Set.cardinal (f.H.app edge) = 2)) paths;
  (* ...and, for functions to the interval, those which are not read-once such as the majority. *)
  let vertex l = Array.of_list (List.map Option.some l) in
  let majority f =
    List.for_all
      (fun (a,b,c) ->
         let v = (a && b) || (a && c) || (b && c) in
         C.Set.equal (f.H.app (vertex [a;b;c])) (C.Set.singleton (vertex [v])))
      (List.concat_map (fun a -> List.concat_map (fun b -> List.map (fun c -> a,b,c) [false;true]) [false;true]) [false;true])
  in
  let majorities = List.filter majority (O.cells 1 3) in
  assert (List.length majorities = 1);
  assert (not (is_box 1 3 (List.hd majorities)));
  (* Functors compose. *)
  List.iter
    (fun f ->
       List.iter
         (fun g ->
            let h = C.comp f g in
            assert (H.check h);
            assert (List.exists (H.eq h) (O.cells 1 1))
         ) (O.cells 1 2)
    ) (O.cells 2 1);
  (* List the 1-cells of the square. *)
  print_string "\n# Functors from the interval to the square\n";
  List.iter (fun f -> print_newline (); print_endline (O.to_string f)) (O.cells 2 1)
