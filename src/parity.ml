(** Parity / augmented directed complexes. *)

(** Presentation of a parity complex. *)
module Pres (G : Alphabet.T) = struct
  type generator = G.t
  module C = Set.Make(G)
  type cell = C.t

  (** List of generators along with dimension, source and target. *)
  type t = (generator * (int * cell * cell)) list

  (** Dimension of a generator. *)
  let dim (s:t) g =
    let n, _, _ = List.assoc g s in
    n

  (** Source of a generator. *)
  let src (s:t) g =
    let _, src, _ = List.assoc g s in
    src

  (** Target of a genertory. *)
  let tgt (s:t) g =
    let _, _, tgt = List.assoc g s in
    tgt

  (** Maximal dimension of a generator. *)
  let max_dim (s:t) = List.fold_left (fun d (g,_) -> max d (dim s g)) (-1) s

  (** Operations on cells. *)
  module Cell = struct
    (** A cell (a homogeneous set of generators). *)
    type t = cell

    (** Dimension of a cell. *)
    let dim s (c:t) =
      if C.is_empty c then -1 else dim s (C.choose c)

    (** The empty cell. *)
    let empty = C.empty

    (** Cell consisting of a generator. *)
    let generator g : t = C.singleton g
  end

  (** Empty parity complex. *)
  let empty : t = []

  (** Add a cell. *)
  let add (s:t) g (src, tgt) : t =
    let n = Cell.dim s src in
    assert (n = Cell.dim s tgt);
    (g,(n+1,src,tgt))::s

  (** Add a point. *)
  let add_point s g : t =
    add (s:t) g (Cell.empty, Cell.empty)
end

(** Presentation on a monoid of generators. *)
module PresTens (G : Monoid.T) = struct
  include Pres(G)

  (* let tensor *)
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
end
