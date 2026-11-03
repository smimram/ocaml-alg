(** Parity / augmented directed complexes. *)

open Extlib

(** Presentation of a parity complex. *)
module Pres (G : Alphabet.T) = struct
  type generator = G.t
  module Map = Map.Make(G)
  module Set = Set.Make(G)
  module Cat = Precategory.Make(G)
  type chain = Set.t

  (** List of generators along with dimension, source and target. *)
  type t = (int * chain * chain) Map.t

  (** Dimension of a generator. *)
  let dim (s:t) g =
    let n, _, _ = Map.find g s in
    n

  (** Source of a generator. *)
  let src (s:t) g =
    let _, src, _ = Map.find g s in
    src

  (** Target of a generator. *)
  let tgt (s:t) g =
    let _, _, tgt = Map.find g s in
    tgt

  (** Fold a function over generators. *)
  let fold f (s:t) = Map.fold (fun g _ d ->  f g d) s

  (** Maximal dimension of a generator. *)
  let max_dim (s:t) = fold (fun g d -> max d (dim s g)) s (-1)

  let _dim = dim
  let _src = src
  let _tgt = tgt

  (** Set of generators of a complex. *)
  let generators ?dim (s:t) =
    let ans = fold Set.add s Set.empty in
    match dim with
    | Some n -> Set.filter (fun g -> _dim s g = n) ans
    | None -> ans

  (** Sorted list of generators. *)
  let to_list (s:t) =
    Map.to_list s
    |> List.map (fun (g,(n,_,_)) -> g, n)
    |> List.sort (fun (g,n) (g',n') -> let c = n - n' in if c = 0 then G.compare g g' else c)
    |> List.map fst

  (** Operations on chains. *)
  module Chain = struct
    (** A chain: a homogeneous set of generators. *)
    type t = chain

    (** Equality. *)
    let eq = Set.equal

    (** Dimension of a chain, by convention -1 for the empty one. *)
    let dim s (c:t) = if Set.is_empty c then -1 else dim s (Set.choose c)

    (** The empty chain. *)
    let empty = Set.empty

    (** Chain consisting of a generator. *)
    let generator g : t = Set.singleton g

    (** Iterate over generators. *)
    let iter = Set.iter

    (** Fold over generators. *)
    let fold = Set.fold

    let sources s c = fold (fun g e -> Set.union (src s g) e) c Set.empty

    let targets s c = fold (fun g e -> Set.union (tgt s g) e) c Set.empty

    (** Whether a chain is well-formed in dimension n: its generators are of dimension n and it is either a single generator (n = 0) or such that distinct generators have disjoint sources and disjoint targets (n > 0). The dimension has to be specified because the empty chain is well-formed exactly in positive dimension. *)
    let well_formed s n (c:t) =
      let disjoint f =
        let rec aux seen = function
          | g::l -> Set.disjoint (f s g) seen && aux (Set.union (f s g) seen) l
          | [] -> true
        in
        aux Set.empty (Set.elements c)
      in
      Set.for_all (fun g -> _dim s g = n) c
      && if n = 0 then Set.cardinal c = 1 else disjoint src && disjoint tgt

    (** Source of a chain. *)
    let src s c = Set.diff (sources s c) (targets s c)

    (** Target of a chain. *)
    let tgt s c = Set.diff (targets s c) (sources s c)
  end

  (** Operations on cells. *)
  module Cell = struct
    (** A cell: its top-dimensional generators, along with its source and target in each lower dimension, from highest to lowest. *)
    type t = Chain.t * (Chain.t * Chain.t) list

    (** Equality. *)
    let eq ((c,b):t) ((c',b'):t) =
      Chain.eq c c'
      && List.compare_lengths b b' = 0
      && List.for_all2 (fun (u,v) (u',v') -> Chain.eq u u' && Chain.eq v v') b b'

    (** Dimension of a cell. *)
    let dim ((_,b):t) = List.length b

    (** The cell whose boundary is entirely determined by a non-empty chain, obtained by iterating sources and targets. *)
    let of_chain s c : t =
      if Set.is_empty c then invalid_arg "Parity.Cell.of_chain: empty chain";
      let rec boundary n u v =
        if n <= 0 then [] else
          let u = Chain.src s u in
          let v = Chain.tgt s v in
          (u, v)::(boundary (n-1) u v)
      in
      c, boundary (Chain.dim s c) c c

    (** Cell consisting of a generator. *)
    let generator s g : t = of_chain s (Chain.generator g)

    (** Source of a cell. *)
    let src ((_,b):t) : t =
      match b with
      | (u,_)::b -> u, b
      | [] -> invalid_arg "Parity.Cell.src: 0-dimensional cell"

    (** Target of a cell. *)
    let tgt ((_,b):t) : t =
      match b with
      | (_,v)::b -> v, b
      | [] -> invalid_arg "Parity.Cell.tgt: 0-dimensional cell"

    (** Iterated source. *)
    let rec isrc c k = if k >= dim c then c else isrc (src c) k

    (** Iterated target. *)
    let rec itgt c k = if k >= dim c then c else itgt (tgt c) k

    (** Identity on a cell. *)
    let id ((c,b):t) : t = Chain.empty, (c,c)::b

    (** Compose two cells along their k-dimensional boundary, by default the highest possible one. *)
    let comp ?k c d : t =
      let k = match k with Some k -> k | None -> min (dim c) (dim d) - 1 in
      if k < 0 || k >= min (dim c) (dim d) then invalid_arg "Parity.Cell.comp: invalid dimension";
      if not (eq (itgt c k) (isrc d k)) then invalid_arg "Parity.Cell.comp: cells are not composable";
      let n = max (dim c) (dim d) in
      let rec lift c = if dim c < n then lift (id c) else c in
      let c, b = lift c in
      let d, b' = lift d in
      let column i ((u,v),(u',v')) =
        let j = n - 1 - i in
        if j < k then u, v
        else if j = k then u, v'
        else Set.union u u', Set.union v v'
      in
      Set.union c d, List.mapi column (List.combine b b')

    (** Categorical decomposition of a cell. *)
    let to_cat s (c:t) : Cat.t =
      (* Apply the generators m to the cell c. *)
      let act c m = Set.union (Set.diff c (Chain.src s m)) (Chain.tgt s m) in
      (* Generators of c which have to occur before those of f. *)
      let before c f =
        let rec aux p =
          let sources = Chain.sources s (Set.union f p) in
          let p' = Set.filter (fun g -> not (Set.disjoint (_tgt s g) sources)) (Set.diff c f) in
          if Set.equal p p' then p else aux p'
        in
        aux Set.empty
      in
      let rec cell (c, b) =
        match b with
        | [] ->
          if Set.cardinal c <> 1 then invalid_arg "Parity.to_cat: expected exactly one 0-dimensional generator";
          let g = Set.choose c in
          let rec x = Cat.C (Cat.G g, Cat.I x) in
          x
        | (u, v)::b ->
          if Set.is_empty c then
            if Set.equal u v then Cat.I (cell (u, b))
            else invalid_arg "Parity.to_cat: source and target do not match"
          else
            match List.find_opt (fun g -> Set.subset (_src s g) u) (Set.elements c) with
            | None -> invalid_arg "Parity.to_cat: no generator can be applied"
            | Some g ->
              let w = whisker g (_src s g) (_tgt s g) (u, b) in
              Cat.C (w, cell (Set.remove g c, (act u (Set.singleton g), v)::b))
      (* Whisker the generator g, whose iterated source and target in the current dimension are f and h, in the context of the cell c. *)
      and whisker g f h (c, b) =
        match b with
        | [] -> Cat.W (cell (f, []), Cat.G g, cell (h, []))
        | (u, v)::b ->
          if not (Set.subset f c) then invalid_arg "Parity.to_cat: generator cannot be whiskered";
          let p = before c f in
          let q = Set.diff (Set.diff c f) p in
          let u' = act u p in
          let u'' = act u' f in
          let w = whisker g (Chain.src s f) (Chain.tgt s h) (u', b) in
          Cat.W (cell (p, (u, u')::b), w, cell (q, (u'', v)::b))
      in
      cell c

    (** String representation. *)
    let to_string s c = Cat.to_string @@ to_cat s c
  end

  (** Empty parity complex. *)
  let empty : t = Map.empty

  (** Add a generator. *)
  let add (s:t) g (src, tgt) : t =
    let n = Chain.dim s src in
    assert (n = Chain.dim s tgt);
    (* The iterated sources and targets of the generator should be well-formed, otherwise it does not have a cell as boundary. *)
    let rec well_formed f n c = Chain.well_formed s n c && (n = 0 || well_formed f (n-1) (f s c)) in
    if n >= 0 && not (well_formed Chain.src n src && well_formed Chain.tgt n tgt) then invalid_arg "Parity.add: source and target are not well-formed";
    if not (Set.disjoint src tgt) then invalid_arg "Parity.add: source and target are not disjoint";
    if not (Chain.eq (Chain.src s src) (Chain.src s tgt) && Chain.eq (Chain.tgt s src) (Chain.tgt s tgt)) then invalid_arg "Parity.add: source and target are not parallel";
    Map.add g (n+1,src,tgt) s

  (** Add a point. *)
  let add_point s g : t =
    add s g (Chain.empty, Chain.empty)

  (** Source and target of a complex in each dimension, from the given one down to 0. *)
  let rec boundary (s:t) n =
    if n < 0 then [] else
      let c = generators ~dim:n s in
      let d = generators ~dim:(n+1) s in
      (Set.diff c (Chain.targets s d), Set.diff c (Chain.sources s d))::(boundary s (n-1))

  let to_string (s:t) =
    to_list s
    |> List.map (fun g -> if dim s g = 0 then G.to_string g else Printf.sprintf "%s : %s -> %s" (G.to_string g) (Cell.to_string s (Cell.of_chain s (src s g))) (Cell.to_string s (Cell.of_chain s (tgt s g))))
    |> String.concat "\n"
end

(** Presentation on a monoid of generators. *)
module PresTens (G : Monoid.T) = struct
  include Pres(G)

  (** Tensor product of two complexes. *)
  let tensor (s:t) (s':t) =
    let n = max_dim s in
    let n' = max_dim s' in
    let ans = ref empty in
    for i = 0 to n + n' do
      for j = 0 to i do
        Chain.iter (fun g ->
            Chain.iter (fun h ->
                let gh = G.mul g h in
                let _src = src in
                let src =
                  Set.union
                    (Set.map (fun g -> G.mul g h) (src s g))
                    (Set.map (fun h -> G.mul g h) ((if dim s g mod 2 = 0 then src else tgt) s' h))
                in
                let tgt =
                  Set.union
                    (Set.map (fun g -> G.mul g h) (tgt s g))
                    (Set.map (fun h -> G.mul g h) ((if dim s g mod 2 = 0 then tgt else _src) s' h))
                in
                ans := add !ans gh (src,tgt)
              ) (generators ~dim:(i-j) s')
          ) (generators ~dim:j s);
      done
    done;
    !ans
end

module Globe = struct
  module A : Alphabet.T with type t = bool * int = struct
    type t = bool * int
    let eq = (=)
    let compare = compare
    let to_string (b,n) = Alphabet.Bool.to_string b ^ String.subscript (string_of_int n)
  end

  include Pres(A)

  (** The n-sphere. *)
  let rec sphere n =
    if n < 0 then empty
    else
      let s = sphere (n-1) in
      let boundary b = if n = 0 then Chain.empty else Chain.generator (b,n-1) in
      let s = add s (false,n) (boundary false, boundary true) in
      let s = add s (true,n) (boundary false, boundary true) in
      s

  (** The n-globe. *)
  let make n =
    if n = 0 then add_point empty (true,0) else
      let g = sphere (n-1) in
      let g = add g (true,n) (Chain.generator (false,n-1), Chain.generator (true,n-1)) in
      g
end

module Cube = struct
  module M = struct
    include Monoid.Free(Alphabet.Option(Alphabet.Bool))

    let zero = inj None

    let top = inj (Some true)

    let bot = inj (Some false)
  end

  include PresTens(M)

  (** The point. *)
  let point = add_point empty M.zero

  (** The interval. *)
  let interval =
    let src = M.bot in
    let tgt = M.top in
    let i = empty in
    let i = add_point i src in
    let i = add_point i tgt in
    let i = add i M.zero (Chain.generator src, Chain.generator tgt) in
    i

  (** Create an n-dimensional cube. *)
  let make n =
    if n = 0 then point else
      let ans = ref interval in
      for _ = 0 to n - 2 do
        ans := tensor !ans interval
      done;
      !ans
end

(*
(** Cubes with degeneracies and connections. *)
module CubeDCon = struct
end
*)

module Oriental = struct
  module A = Alphabet.List(Alphabet.Int)
  include Pres(A)

  (** The n-oriental (oriented n-simplex). *)
  let make n =
    (* Strictly increasing lists of len integers between pos and n (both included). *)
    let rec subsets pos len =
      if len = 0 then [[]]
      else if n + 1 - pos < len then []
      else
        List.init (n + 1 - pos) (fun i -> pos + i)
        |> List.concat_map (fun x -> List.map (fun l -> x::l) (subsets (x+1) (len-1)))
    in
    (* The faces of a simplex obtained by removing an element whose position has the given parity. *)
    let faces parity l =
      List.mapi (fun i _ -> i) l
      |> List.filter (fun i -> i mod 2 = parity)
      |> List.map (fun i -> List.filteri (fun j _ -> j <> i) l)
      |> Set.of_list
    in
    let s = ref empty in
    for i = 0 to n do
      List.iter
        (fun l ->
           s :=
             if i = 0 then add_point !s l
             else add !s l (faces 1 l, faces 0 l)
        ) (subsets 0 (i+1))
    done;
    !s
end
