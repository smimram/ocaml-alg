(** Parity / augmented directed complexes. *)

(** Presentation of a parity complex. *)
module Pres (G : Alphabet.T) = struct
  type generator = G.t
  module Map = Map.Make(G)
  module Set = Set.Make(G)
  type cell = Set.t

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

  (** Target of a genertory. *)
  let tgt (s:t) g =
    let _, _, tgt = Map.find g s in
    tgt

  (** Maximal dimension of a generator. *)
  let max_dim (s:t) = Map.fold (fun g _ d -> max d (dim s g)) s (-1)

  let _dim = dim

  (** Set of generators of a complex. *)
  let generators ?dim (s:t) =
    let ans = Map.fold (fun g _ set -> Set.add g set) s Set.empty in
    match dim with
    | Some n -> Set.filter (fun g -> _dim s g = n) ans
    | None -> ans

  (** Operations on cells. *)
  module Cell = struct
    (** A cell (a homogeneous set of generators). *)
    type t = cell

    (** Dimension of a cell. *)
    let dim s (c:t) =
      if Set.is_empty c then -1 else dim s (Set.choose c)

    (** The empty cell. *)
    let empty = Set.empty

    (** Cell consisting of a generator. *)
    let generator g : t = Set.singleton g

    let iter = Set.iter
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
              ) (generators ~dim:(j-i) s')
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
