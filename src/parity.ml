(** Parity / augmented directed complexes. *)

open Extlib

(** A parity complex. *)
module type T = sig
  (** A generator. *)
  type generator

  (** String representation of a generator. *)
  val generator_to_string : generator -> string

  (** Maps from generators. *)
  module Map : Map.S with type key = generator

  (** A complex. *)
  type t

  (** Operations on chains. *)
  module Chain : sig
    type complex := t

    (** A chain is a list of generators of the same dimension. *)
    type t

    (** Equality. *)
    val eq : t -> t -> bool

    (** Comparison. *)
    val compare : t -> t -> int

    (** String representation. *)
    val to_string : t -> string

    (** The empty chain. *)
    val empty : t

    (** A chain consisting of a single generator. *)
    val generator : generator -> t

    (** Number of generators. *)
    val cardinal : t -> int

    (** Fold over generators. *)
    val fold : (generator -> 'a -> 'a) -> t -> 'a -> 'a

    (** Whether all generators satisfy a predicate. *)
    val for_all : (generator -> bool) -> t -> bool

    (** Generators occurring in either chain. *)
    val union : t -> t -> t

    (** Generators of the first chain which do not occur in the second one. *)
    val diff : t -> t -> t

    (** Source of a chain. *)
    val src : complex -> t -> t

    (** Target of a chain. *)
    val tgt : complex -> t -> t
  end

  (** Dimension of a generator. *)
  val dim : t -> generator -> int

  (** Source of a generator. *)
  val src : t -> generator -> Chain.t

  (** Target of a generator. *)
  val tgt : t -> generator -> Chain.t

  (** Sorted list of generators. *)
  val to_list : t -> generator list

  (** Operations on cells. *)
  module Cell : sig
    type complex := t

    (** A cell. *)
    type t

    (** Create a cell from its top-dimensional generators, along with its source and target in each lower dimension, from highest to lowest. *)
    val make : Chain.t -> (Chain.t * Chain.t) list -> t

    (** The top-dimensional generators of a cell. *)
    val chain : t -> Chain.t

    (** The source and target of a cell in each lower dimension, from highest to lowest. *)
    val boundary : t -> (Chain.t * Chain.t) list

    (** Equality. *)
    val eq : t -> t -> bool

    (** Comparison. *)
    val compare : t -> t -> int

    (** Dimension of a cell. *)
    val dim : t -> int

    (** The cell whose boundary is entirely determined by a non-empty chain, obtained by iterating sources and targets. *)
    val of_chain : complex -> Chain.t -> t

    (** Cell consisting of a generator. *)
    val generator : complex -> generator -> t

    (** Source of a cell. *)
    val src : t -> t

    (** Target of a cell. *)
    val tgt : t -> t

    (** Iterated source. *)
    val isrc : t -> int -> t

    (** Iterated target. *)
    val itgt : t -> int -> t

    (** Identity on a cell. *)
    val id : t -> t

    (** Compose two cells along their k-dimensional boundary, by default the highest possible one. *)
    val comp : ?k:int -> t -> t -> t

    (** String representation. *)
    val to_string : complex -> t -> string
  end

  (** All the cells of the generated free category in each dimension, up to the given one. *)
  val cells : t -> int -> Cell.t list array
end

(** Presentation of a parity complex. *)
module Pres (G : Alphabet.T) = struct
  type generator = G.t
  let generator_to_string = G.to_string
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

    (** Comparison. *)
    let compare = Set.compare

    (** String representation. *)
    let to_string c =
      "{" ^ String.concat ", " (List.map G.to_string (Set.elements c)) ^ "}"

    (** Dimension of a chain, by convention -1 for the empty one. *)
    let dim s (c:t) = if Set.is_empty c then -1 else dim s (Set.choose c)

    (** The empty chain. *)
    let empty = Set.empty

    (** Chain consisting of a generator. *)
    let generator g : t = Set.singleton g

    (** Number of generators. *)
    let cardinal = Set.cardinal

    (** Iterate over generators. *)
    let iter = Set.iter

    (** Fold over generators. *)
    let fold = Set.fold

    (** Whether all generators satisfy a predicate. *)
    let for_all = Set.for_all

    (** Generators occurring in either chain. *)
    let union = Set.union

    (** Generators of the first chain which do not occur in the second one. *)
    let diff = Set.diff

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

    (** Create a cell from its top-dimensional generators, along with its source and target in each lower dimension, from highest to lowest. *)
    let make c b : t = c, b

    (** The top-dimensional generators of a cell. *)
    let chain ((c,_):t) = c

    (** The source and target of a cell in each lower dimension, from highest to lowest. *)
    let boundary ((_,b):t) = b

    (** Equality. *)
    let eq ((c,b):t) ((c',b'):t) =
      Chain.eq c c'
      && List.compare_lengths b b' = 0
      && List.for_all2 (fun (u,v) (u',v') -> Chain.eq u u' && Chain.eq v v') b b'

    (** Comparison. *)
    let compare ((c,b):t) ((c',b'):t) =
      let pair (u,v) (u',v') =
        let n = Chain.compare u u' in
        if n <> 0 then n else Chain.compare v v'
      in
      let n = Chain.compare c c' in
      if n <> 0 then n else List.compare pair b b'

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

  (** All the cells of the generated free category in each dimension, up to the given one: in dimension m, these are obtained from the generators and the identities on cells of dimension m-1 by closing under composition. *)
  let cells (s:t) d =
    let module C = Stdlib.Set.Make(Cell) in
    let module M = Stdlib.Map.Make(Cell) in
    let ans = Array.make (d+1) [] in
    for m = 0 to d do
      let seen = ref C.empty in
      (* The cells known so far, by iterated source and target in each dimension. *)
      let by_src = Array.make m M.empty in
      let by_tgt = Array.make m M.empty in
      let find t k c = Option.value ~default:[] (M.find_opt c t.(k)) in
      let add t k c x = t.(k) <- M.add c (x :: find t k c) t.(k) in
      let rec visit x =
        if not (C.mem x !seen) then
          (
            seen := C.add x !seen;
            for k = 0 to m - 1 do
              add by_src k (Cell.isrc x k) x;
              add by_tgt k (Cell.itgt x k) x
            done;
            for k = 0 to m - 1 do
              List.iter (fun y -> visit (Cell.comp ~k x y)) (find by_src k (Cell.itgt x k));
              List.iter (fun y -> visit (Cell.comp ~k y x)) (find by_tgt k (Cell.isrc x k))
            done
          )
      in
      List.iter (fun g -> visit (Cell.generator s g)) (Set.elements (generators ~dim:m s));
      if m > 0 then List.iter (fun c -> visit (Cell.id c)) ans.(m-1);
      ans.(m) <- C.elements !seen
    done;
    ans

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

module PresT (G : Monoid.T) : T = Pres(G)

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
  (** Our alphabet. *)
  module A = struct
    include Monoid.Free(Alphabet.Option(Alphabet.Bool))

    let zero = inj None

    let top = inj (Some true)

    let bot = inj (Some false)
  end

  include PresTens(A)

  (** The point. *)
  let point = add_point empty A.one

  (** The interval. *)
  let interval =
    let src = A.bot in
    let tgt = A.top in
    let i = empty in
    let i = add_point i src in
    let i = add_point i tgt in
    let i = add i A.zero (Chain.generator src, Chain.generator tgt) in
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

(** Pasting schemes. *)
module PastingScheme = struct
  (** A generator is a dimension along with a number. *)
  module A : Alphabet.T with type t = int * int = struct
    type t = int * int
    let eq = (=)
    let compare = compare
    let to_string (n,i) = string_of_int i ^ String.subscript (string_of_int n)
  end

  include Pres(A)

  (** The point. *)
  let point = add_point empty (0,0)

  (** Suspension of a complex: every generator goes up by one dimension, and the former points now go between two new points. *)
  let susp s =
    let bot, top = (0,0), (0,1) in
    let shift (n,i) = n+1, i in
    let bnd g =
      if dim s g = 0 then Chain.generator bot, Chain.generator top
      else Set.map shift (src s g), Set.map shift (tgt s g)
    in
    List.fold_left
      (fun r g -> add r (shift g) (bnd g))
      (add_point (add_point empty bot) top)
      (to_list s)

  (** Wedge of two complexes: the last point of the first one, which has to be its target, is identified with the point 0 of the second one, which has to be its source. Generators of the second complex are numbered after those of the first. *)
  let wedge s s' =
    (* The offset for the numbers of the generators of s' in each dimension. *)
    let offset =
      Array.init (1 + max 0 (max (max_dim s) (max_dim s')))
        (fun n ->
           let k = Set.cardinal (generators ~dim:n s) in
           if n = 0 then k - 1 else k)
    in
    let endpoints s = List.hd (boundary s 0) in
    if not (Set.equal (snd (endpoints s)) (Chain.generator (0,offset.(0))) && Set.equal (fst (endpoints s')) (Chain.generator (0,0))) then
      invalid_arg "Parity.PastingScheme.wedge: endpoints do not match";
    let shift (n,i) = n, i + offset.(n) in
    List.fold_left
      (fun r g -> add r (shift g) (Set.map shift (src s' g), Set.map shift (tgt s' g)))
      s
      (to_list s')

  (** The parity complex associated to a pasting scheme. *)
  let rec make p =
    List.fold_left (fun s p -> wedge s (susp (make p))) point (PastingScheme.to_list p)

  (** The cell consisting of a whole pasting scheme. *)
  let cell s : Cell.t =
    let n = max_dim s in
    generators ~dim:n s, boundary s (n-1)
end

(** Morphisms between complexes, as maps of augmented directed complexes, which induce functors between the generated free categories. *)
module Morphism (P:T) (Q:T) = struct
  (** A morphism sends a generator to a chain of the same dimension, which is empty when the generator collapses to a lower dimension. *)
  type t =
    {
      dom : P.t; (** domain *)
      cod : Q.t; (** codomain *)
      app : P.generator -> Q.Chain.t; (** image of a generator *)
    }

  (** Create a morphism from its action on generators. *)
  let make dom cod app = { dom; cod; app }

  (** Create a morphism sending each generator to a generator, or collapsing it when the image is [None]. *)
  let of_partial dom cod f =
    make dom cod (fun g -> match f g with Some g' -> Q.Chain.generator g' | None -> Q.Chain.empty)

  (** Image of a chain. *)
  let chain f c = P.Chain.fold (fun g e -> Q.Chain.union (f.app g) e) c Q.Chain.empty

  (** Image of a cell, given by its generators and its source and target in each lower dimension. The dimension of the cell is preserved, so that it might become an identity. *)
  let cell f (c:P.Cell.t) : Q.Cell.t =
    Q.Cell.make (chain f (P.Cell.chain c)) (List.map (fun (u,v) -> chain f u, chain f v) (P.Cell.boundary c))

  (** Whether two morphisms with the same domain coincide on generators. *)
  let eq f g = List.for_all (fun x -> Q.Chain.eq (f.app x) (g.app x)) (P.to_list f.dom)

  (** Whether the images of the generators of a chain are disjoint. Otherwise, the image of the chain is not faithfully represented by a set. *)
  let injective_on f c =
    P.Chain.fold (fun g n -> n + Q.Chain.cardinal (f.app g)) c 0 = Q.Chain.cardinal (chain f c)

  (** Check that the action on generators defines a morphism: it preserves dimensions and augmentation and commutes with the boundary. The boundary has to be computed with integer coefficients since, contrarily to what happens for well-formed chains, generators of an arbitrary chain can share a source or a target, and sets would forget this. *)
  let check f =
    (* Add a chain, with given coefficient, to a linear combination of generators. *)
    let add n c m =
      Q.Chain.fold
        (fun g m ->
           let n = n + Option.value ~default:0 (Q.Map.find_opt g m) in
           if n = 0 then Q.Map.remove g m else Q.Map.add g n m
        ) c m
    in
    let boundary c =
      Q.Chain.fold (fun g m -> add 1 (Q.tgt f.cod g) (add (-1) (Q.src f.cod g) m)) c Q.Map.empty
    in
    List.for_all
      (fun g ->
         let n = P.dim f.dom g in
         let c = f.app g in
         Q.Chain.for_all (fun g' -> Q.dim f.cod g' = n) c
         &&
         if n = 0 then Q.Chain.cardinal c = 1
         else
           let s, t = P.src f.dom g, P.tgt f.dom g in
           let s', t' = chain f s, chain f t in
           injective_on f s
           && injective_on f t
           && Q.Map.equal (=) (add 1 t' (add (-1) s' Q.Map.empty)) (boundary c)
      ) (P.to_list f.dom)

  (** All the morphisms between two complexes, which are the functors between the generated free categories. Since the domain is free, such a functor amounts to choosing, for each generator by increasing dimension, a cell whose boundary is the image of the boundary of the generator. *)
  let all dom cod =
    let module B =
      Map.Make(struct
        type t = (Q.Chain.t * Q.Chain.t) list
        let compare =
          List.compare
            (fun (u,v) (u',v') ->
               let n = Q.Chain.compare u u' in
               if n <> 0 then n else Q.Chain.compare v v')
      end)
    in
    let generators = List.map (fun g -> g, P.dim dom g, P.Cell.boundary (P.Cell.generator dom g)) (P.to_list dom) in
    let d = List.fold_left (fun d (_,n,_) -> max d n) 0 generators in
    (* The chains of the cells in each dimension, by boundary. *)
    let index =
      Array.map
        (List.fold_left
           (fun i c ->
              let b = Q.Cell.boundary c in
              B.add b (Q.Cell.chain c :: Option.value ~default:[] (B.find_opt b i)) i
           ) B.empty)
        (Q.cells cod d)
    in
    let rec aux m = function
      | [] -> [m]
      | (g,n,b)::generators ->
        let image c = P.Chain.fold (fun x e -> Q.Chain.union (P.Map.find x m) e) c Q.Chain.empty in
        let b = List.map (fun (u,v) -> image u, image v) b in
        Option.value ~default:[] (B.find_opt b index.(n))
        |> List.concat_map (fun c -> aux (P.Map.add g c m) generators)
    in
    List.map (fun m -> make dom cod (fun g -> P.Map.find g m)) (aux P.Map.empty generators)

  (** String representation, as the image of each generator. *)
  let to_string f =
    P.to_list f.dom
    |> List.map (fun g -> P.generator_to_string g ^ " ↦ " ^ Q.Chain.to_string (f.app g))
    |> String.concat "\n"
end

(** Composition of morphisms. *)
module MorphismComp (P:T) (Q:T) (R:T) = struct
  module F = Morphism(P)(Q)
  module G = Morphism(Q)(R)
  module H = Morphism(P)(R)

  (** Composite of two morphisms: the first one is applied first. *)
  let comp (f:F.t) (g:G.t) : H.t =
    H.make f.dom g.cod (fun x -> G.chain g (f.app x))
end

(** Morphisms between cubes, which come in two flavors: the ones of the box category ({!Box}), and the functors between the generated free categories ({!Omega}), the former being particular cases of the latter. The operations on words below describe a map by its action on generators: it sends a generator to a generator of the same dimension, or to [None] when the generator collapses to a lower dimension. Use {!Hom.of_partial} to obtain the corresponding morphism. Coordinates are numbered from 0. *)
module CubeMorphism = struct
  include Cube

  (** Morphisms between cubes. *)
  module Hom = Morphism(Cube)(Cube)

  (** Composite of morphisms between cubes. *)
  let comp =
    let module C = MorphismComp(Cube)(Cube)(Cube) in
    C.comp

  (** The word obtained by replacing the coordinates from i to i+k-1 of x by the word v. *)
  let splice x i k v =
    Array.concat [Array.sub x 0 i; v; Array.sub x (i+k) (Array.length x - i - k)]

  (** Face: insert a coordinate i, which is the target if e is true and the source otherwise. *)
  let face e i x = Some (splice x i 0 (if e then A.top else A.bot))

  (** Degeneracy: forget the coordinate i. *)
  let deg i x =
    if x.(i) = None then None else Some (splice x i 1 A.one)

  (** Connection: merge the coordinates i and i+1, by taking their maximum if first argument is true and their minimum otherwise. *)
  let con up i x =
    let merged =
      match x.(i), x.(i+1) with
      | None, None -> None
      | None, Some b | Some b, None -> if b = up then None else Some None
      | Some a, Some b -> Some (Some (if up then a || b else a && b))
    in
    Option.map (fun v -> splice x i 2 [|v|]) merged

  (** The box category with connections: the maps generated by faces, degeneracies and connections (for both maximum and minimum). These are the cells of the cubes in cubical sets with connections: a k-cell of the n-cube is a map from the k-cube to the n-cube, which is described by a word of length n over the free bounded distributive lattice on the variables x₀, ..., xₖ₋₁: the letter at position i gives the value of the coordinate i. Not all words are cells: for instance, the cells contain neither symmetries nor diagonals. Since a word does not determine k, the dimension has to be specified where it matters. *)
  module Box = struct
    (** Variables. *)
    module Var = struct
      include Alphabet.Int

      let to_string i = "x" ^ String.subscript (string_of_int i)
    end

    (** Values of the coordinates. *)
    module D = struct
      include Lattice.FreeBoundedDistributiveLattice(Var)

      let to_string x = to_string x
    end

    include Monoid.Free(D)

    (** The value of a constant coordinate, which is the target if e is true and the source otherwise. *)
    let const e = if e then D.top else D.bot

    (** Maximum if e is true and minimum otherwise. *)
    let merge e = if e then D.sup else D.inf

    (** The top cell of the k-cube. *)
    let id k : t = Array.init k D.inj

    (** Composite of a k-cell of the m-cube with an m-cell of the n-cube. *)
    let comp (f:t) (g:t) : t = Array.map (D.subst (fun i -> f.(i))) g

    (** Face of a cell in direction i. *)
    let face e i : t -> t =
      Array.map (D.subst (fun j -> if j < i then D.inj j else if j = i then const e else D.inj (j-1)))

    (** Degeneracy of a cell, with a new variable i. *)
    let deg i : t -> t =
      Array.map (D.subst (fun j -> D.inj (if j < i then j else j+1)))

    (** Connection of a cell, which replaces the variable i by the merge of the new variables i and i+1. *)
    let con e i : t -> t =
      Array.map (D.subst (fun j -> if j < i then D.inj j else if j = i then merge e (D.inj i) (D.inj (i+1)) else D.inj (j+1)))

    (** The action of a cell on generators: a generator is sent to the face spanned by the image of its lowest and highest vertices, and collapses when this lowers its dimension. *)
    let map (w:t) (g:A.t) =
      let vertex d i = match g.(i) with Some b -> b | None -> d in
      let coordinate x =
        let a = D.eval_bool (vertex false) x in
        if a = D.eval_bool (vertex true) x then Some a else None
      in
      let g' = Array.map coordinate w in
      let dim x = Array.fold_left (fun n a -> if a = None then n+1 else n) 0 x in
      if dim g' = dim g then Some g' else None

    (** The morphism from the k-cube dom to the n-cube cod corresponding to a k-cell of the n-cube. *)
    let hom dom cod (w:t) = Hom.of_partial dom cod (map w)

    (** All composites of degeneracies and connections from the k-cube, indexed by the dimension of their codomain. *)
    let collapses k =
      let ans = Array.make (k+1) [] in
      ans.(k) <- [id k];
      for m = k downto 1 do
        let next w =
          List.init m (fun i -> splice w i 1 [||])
          @ List.concat_map (fun e -> List.init (m-1) (fun i -> splice w i 2 [|merge e w.(i) w.(i+1)|])) [false; true]
        in
        ans.(m-1) <- List.sort_uniq compare @@ List.concat_map next ans.(m)
      done;
      ans

    (** The k-cells of the n-cube. Each one uniquely factors as a collapse followed by the inclusion of a face. *)
    let cells n k : t list =
      let inject g w =
        let j = ref 0 in
        Array.map (function None -> let a = w.(!j) in incr j; a | Some e -> const e) g
      in
      let collapses = collapses k in
      let c = make n in
      List.concat_map
        (fun m ->
           if m > k then [] else
             List.concat_map
               (fun g -> List.map (inject g) collapses.(m))
               (Set.elements (generators ~dim:m c))
        ) (List.init (n+1) Fun.id)

    (** String representation. *)
    let to_string (w:t) =
      "(" ^ String.concat " , " (List.map D.to_string (Array.to_list w)) ^ ")"
  end

  (** Functors between the free categories generated by cubes. These are the cells of the cubical nerve of the free category on a cube. They strictly contain the maps of {!Box}: a generator can be sent to a composite, for instance an edge to a path around a square, and the images of vertices are arbitrary monotone maps. *)
  module Omega = struct
    (** The k-cells of the n-cube: the functors from the k-cube to the n-cube. *)
    let cells n k = Hom.all (make k) (make n)

    (** String representation. *)
    let to_string = Hom.to_string
  end
end
