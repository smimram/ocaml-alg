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

  (** Set of generators of a complex. *)
  let generators ?dim (s:t) =
    let ans = fold Set.add s Set.empty in
    match dim with
    | Some n -> Set.filter (fun g -> _dim s g = n) ans
    | None -> ans

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
      assert (eq (itgt s c k) (isrc s d k));
      Set.union c d
  end

  (** Empty parity complex. *)
  let empty : t = Map.empty

  (** Add a cell. *)
  let add (s:t) g (src, tgt) : t =
    let n = Cell.dim s src in
    assert (n = Cell.dim s tgt);
    Map.add g (n+1,src,tgt) s

  (** Add a point. *)
  let add_point s g : t =
    add (s:t) g (Cell.empty, Cell.empty)

  (** Source and target of a complex in each dimension, from the given one down to 0. *)
  let rec boundary (s:t) n =
    if n < 0 then [] else
      let c = generators ~dim:n s in
      let d = generators ~dim:(n+1) s in
      (Set.diff c (Cell.targets s d), Set.diff c (Cell.sources s d))::(boundary s (n-1))

  (** Make sure that a complex is a valid pasting diagram. Raises [Invalid_argument] if this is not the case. *)
  let check (s:t) =
    let error m = invalid_arg ("Parity.check: " ^ m) in
    (* Make sure that a cell, given with its source and target in each lower dimension, is well-formed: starting from the source, its generators can be applied one after the other in order to reach the target, and the same holds for all the intermediate cells. *)
    let rec composite (c, b) =
      match b with
      | [] -> if Set.cardinal c <> 1 then error "expected exactly one 0-dimensional generator"
      | (u, v)::b ->
        composite (u, b);
        let applicable u g = Set.subset (src s g) u && Set.disjoint (tgt s g) (Set.diff u (src s g)) in
        let rec aux u c =
          if Set.is_empty c then
            (if not (Set.equal u v) then error "source and target do not match")
          else
            match List.find_opt (applicable u) (Set.elements c) with
            | None -> error "no generator can be applied"
            | Some g ->
              let u = Set.union (Set.diff u (src s g)) (tgt s g) in
              composite (u, b);
              aux u (Set.remove g c)
        in
        aux u c
    in
    (* Each generator has a boundary of the expected dimension. *)
    Map.iter
      (fun g (n, f, h) ->
         let name = G.to_string g in
         if (n = 0) <> Set.is_empty f || (n = 0) <> Set.is_empty h then error ("invalid boundary for " ^ name);
         Set.iter
           (fun x -> if not (Map.mem x s) || dim s x <> n - 1 then error ("invalid boundary for " ^ name))
           (Set.union f h);
         if not (Set.disjoint f h) then error ("source and target intersect for " ^ name)
      ) s;
    (* The source and target of each generator are parallel well-formed cells. *)
    Map.iter
      (fun _ (n, f, h) ->
         let rec bdry f h =
           if Cell.dim s f <= 0 then [] else
             let f = Cell.src s f in
             let h = Cell.tgt s h in
             (f, h)::(bdry f h)
         in
         if n > 0 then
           let b = bdry f h in
           composite (f, b);
           composite (h, b)
      ) s;
    (* The whole complex is a well-formed cell. *)
    let n = max_dim s in
    if n < 0 then error "empty complex";
    composite (generators ~dim:n s, boundary s (n-1))

  (** Categorical composition of all the generators of a complex, which is expected to be a pasting diagram. *)
  let to_cat (s:t) : Cat.t =
    check s;
    (* We manipulate a composite as a pair consisting of its top-dimensional generators and of the source and target of the composite in each lower dimension, from highest to lowest. *)
    (* Apply the generators m to the cell c. *)
    let act c m = Set.union (Set.diff c (Cell.src s m)) (Cell.tgt s m) in
    (* Generators of c which have to occur before those of f. *)
    let before c f =
      let rec aux p =
        let sources = Cell.sources s (Set.union f p) in
        let p' = Set.filter (fun g -> not (Set.disjoint (tgt s g) sources)) (Set.diff c f) in
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
          match List.find_opt (fun g -> Set.subset (src s g) u) (Set.elements c) with
          | None -> invalid_arg "Parity.to_cat: no generator can be applied"
          | Some g ->
            let w = whisker g (src s g) (tgt s g) (u, b) in
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
        let w = whisker g (Cell.src s f) (Cell.tgt s h) (u', b) in
        Cat.W (cell (p, (u, u')::b), w, cell (q, (u'', v)::b))
    in
    let n = max_dim s in
    if n < 0 then invalid_arg "Parity.to_cat: empty complex";
    cell (generators ~dim:n s, boundary s (n-1))
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
    let ans = ref point in
    for _ = 0 to n - 1 do
      ans := tensor !ans interval
    done;
    !ans
end
