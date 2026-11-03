(** Parity / augmented directed complexes. *)

(** Presentation of a parity complex. *)
module Pres (G : Alphabet.T) = struct
  type generator = G.t
  module Map = Map.Make(G)
  module Set = Set.Make(G)
  type cell = Set.t
  module Cat = Precategory.Make(G)

  (** List of generators along with dimension, source and target. *)
  type t = (int * cell * cell) Map.t

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

  (** Operations on cells. *)
  module Cell = struct
    (** A cell (a homogeneous set of generators). *)
    type t = cell

    (** Equality. *)
    let eq = Set.equal

    (** Dimension of a cell. *)
    let dim s (c:t) = if Set.is_empty c then -1 else dim s (Set.choose c)

    (** The empty cell. *)
    let empty = Set.empty

    (** Cell consisting of a generator. *)
    let generator g : t = Set.singleton g

    (** Iterate over generators. *)
    let iter = Set.iter

    (** Fold over generators. *)
    let fold = Set.fold

    let sources s c = fold (fun g e -> Set.union (src s g) e) c Set.empty

    let targets s c = fold (fun g e -> Set.union (tgt s g) e) c Set.empty

    (** Source of a cell. *)
    let src s c = Set.diff (sources s c) (targets s c)

    (** Target of a cell. *)
    let tgt s c = Set.diff (targets s c) (sources s c)

    (** Iterated source. *)
    let rec isrc s c k =
      let n = dim s c in
      if k >= n then c else isrc s (src s c) k

    (** Iterated target. *)
    let rec itgt s c k =
      let n = dim s c in
      if k >= n then c else itgt s (tgt s c) k

    (** Compose two cells. *)
    let comp s c d =
      let k = min (dim s c) (dim s d) - 1 in
      assert (k >= 0);
      assert (eq (itgt s c k) (isrc s d k));
      Set.union c d

    (** Categorical decomposition of a cell. *)
    let to_cat s c : Cat.t =
      (* We manipulate a composite as a pair consisting of its top-dimensional generators and of the source and target of the composite in each lower dimension, from highest to lowest. *)
      (* Apply the generators m to the cell c. *)
      let act c m = Set.union (Set.diff c (src s m)) (tgt s m) in
      (* Generators of c which have to occur before those of f. *)
      let before c f =
        let rec aux p =
          let sources = sources s (Set.union f p) in
          let p' = Set.filter (fun g -> not (Set.disjoint (_tgt s g) sources)) (Set.diff c f) in
          if Set.equal p p' then p else aux p'
        in
        aux Set.empty
      in
      (* Iterated sources and targets of the parallel cells u and v. *)
      let rec boundary u v =
        if dim s u <= 0 then [] else
          let u = src s u in
          let v = tgt s v in
          (u, v)::(boundary u v)
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
          let w = whisker g (src s f) (tgt s h) (u', b) in
          Cat.W (cell (p, (u, u')::b), w, cell (q, (u'', v)::b))
      in
      if Set.is_empty c then invalid_arg "Parity.to_cat: empty cell";
      cell (c, boundary c c)

    (** String representation. *)
    let to_string s c = Cat.to_string @@ to_cat s c
  end

  (** Empty parity complex. *)
  let empty : t = Map.empty

  (** Add a generator. *)
  let add (s:t) g (src, tgt) : t =
    let n = Cell.dim s src in
    assert (n = Cell.dim s tgt);
    Map.add g (n+1,src,tgt) s

  (** Add a point. *)
  let add_point s g : t =
    add s g (Cell.empty, Cell.empty)

  (** Source and target of a complex in each dimension, from the given one down to 0. *)
  let rec boundary (s:t) n =
    if n < 0 then [] else
      let c = generators ~dim:n s in
      let d = generators ~dim:(n+1) s in
      (Set.diff c (Cell.targets s d), Set.diff c (Cell.sources s d))::(boundary s (n-1))

  let to_string (s:t) =
    to_list s
    |> List.map (fun g -> if dim s g = 0 then G.to_string g else Printf.sprintf "%s : %s -> %s" (G.to_string g) (Cell.to_string s (src s g)) (Cell.to_string s (tgt s g)))
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
        Cell.iter (fun g ->
            Cell.iter (fun h ->
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

module Cube = struct
  module M = struct
    include Monoid.Free(Alphabet.Option(Alphabet.Bool))

    let zero = inj None

    let top = inj (Some true)

    let bot = inj (Some false)
  end

  include PresTens (M)

  (** The point. *)
  let point = add_point empty M.zero

  (** The interval. *)
  let interval =
    let src = M.bot in
    let tgt = M.top in
    let i = empty in
    let i = add_point i src in
    let i = add_point i tgt in
    let i = add i M.zero (Cell.generator src, Cell.generator tgt) in
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
