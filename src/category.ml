(** Categories. *)

open Extlib

(** A category. *)
module type T = sig
  include Graph.T

  (** Sequential composition of morphisms. *)
  val comp : E.t -> E.t -> E.t

  (** Identity morphism. *)
  val id : V.t -> E.t
end

(** Free category on a graph. *)
module Free (G : Graph.T) = struct
  module V = G.V
  module M = Monoid.Free(G.E)
  module E = struct
    include Alphabet.Prod3(V)(M)(V)
    let to_string (x,f,y) =
      if M.eq M.one f then "id"^V.to_string x else M.to_string f
  end
  let src ((x,f,y):E.t) = x
  let tgt ((x,f,y):E.t) = y
  let id (x:V.t) : E.t = (x,M.one,x)
  let comp ((x,f,y):E.t) ((y',g,z):E.t) : E.t =
    assert (V.eq y y');
    x,M.mul f g,z
  let inj f : E.t = (G.src f,M.inj f,G.tgt f)
end
module FreeCategory (G : Graph.T) : T = Free(G)

(** Opposite category. *)
module Op (C : T) : T = struct
  module V = C.V
  module E = C.E
  let src f = C.tgt f
  let tgt f = C.src f
  let comp f g = C.comp g f
  let id x = C.id x
end

(** Presentation of a category. *)
module Pres (V : Alphabet.T) (E : Alphabet.T) = struct
  module GP = Graph.Pres(V)(E)
  module Free = Free(GP.Graph)

  (** A presentation. *)
  type t =
    {
      graph : GP.t;
      relations : (Free.E.t * Free.E.t) list;
    }

  (** Empty presentation. *)
  let empty = { graph = GP.empty; relations = [] }

  let add_object p x =
    { p with graph = GP.add_vertex p.graph x }

  let add_morphism p f x y =
    assert (GP.has_vertex p.graph x);
    assert (GP.has_vertex p.graph y);
    { p with graph = GP.add_edge p.graph f (x,y) }

  (** Morphism corresponding to a generator. *)
  let morphism p f : Free.E.t =
    Free.inj (GP.edge p.graph f)

  let add_relation p f g =
    { p with relations = (f,g)::p.relations }

  let to_string p =
    let graph = GP.to_string p.graph in
    let relations = p.relations in
    let relations = List.map (fun (f,g) -> Free.E.to_string f ^ "=" ^ Free.E.to_string g) relations in
    let relations = String.concat " , " relations in
    graph ^ "\n" ^ relations
end

(** Category of a monoid. *)
module Monoid (M : Monoid.T) : T = struct
  module V = Alphabet.Unit
  module E = M
  let src _ = ()
  let tgt _ = ()
  let comp = M.mul
  let id () = M.one
end

(** The (augmented or not) simplicial category. *)
module Simplicial = struct
  module V = Alphabet.Nat
  module E = struct
    type nat = int

    (* We encode a morphism as the number of preimages for each number in the target. *)
    type t = nat list

    let src (f:t) = List.fold_left (+) 0 f

    (* IMPORTANT NOTE: we take the convention that we count the number of elements in the source or target! So off by one compared to traditional convention in topology. *)
    let tgt (f:t) = List.length f

    let eq f g =
      assert (src f = src g);
      assert (tgt f = tgt g);
      f = g

    let compare (f:t) (g:t) = compare f g

    let to_string f = String.concat "|" @@ List.map string_of_int f
  end

  let src = E.src

  let tgt = E.tgt

  (** Create a morphism from a function. *)
  let from_fun m n f : E.t =
    let rec aux i j k =
      if i >= m then k::[]
      else if j >= n then []
      else if f i = j then aux (i+1) j (k+1)
      else k::(aux i (j+1) 0)
    in
    aux 0 0 0

  (** Apply a morphism as a function. *)
  let ap f i =
    assert (0 <= i && i < src f);
    let rec aux j from f i =
      match f with
      | k::f -> if i < from+k then j else aux (j+1) (from+k) f i
      | [] -> assert false
    in
    aux 0 0 f i

  let to_fun = ap

  let id n : E.t = List.init n (fun _ -> 1)

  (** Degeneracies. *)
  let degeneracy n i : E.t =
    assert (0 <= i && i <= n);
    id i @ [2] @ id (n-i)

  (** Faces. *)
  let face n i : E.t =
    assert (0 <= i && i <= n);
    id i @ [0] @ id (n-i)

  (** Composition. *)
  let comp f g =
    assert (tgt f = src g);
    let f, g = List.fold_left_map (fun f n -> List.drop n f, List.take n f) f g in
    assert (f = []);
    List.map (List.fold_left (+) 0) g

  (** Every simplicial morphism m → n induces an "interval" map n+1 → m+1. *)
  let interval (f:E.t) =
    (* TODO: better implementation? *)
    let rec aux j i =
      if j = src f then j
      else if ap f j >= i then j else aux (j+1) i
    in
    let g = aux 0 in
    from_fun (tgt f + 1) (src f + 1) g
end

module SimplicialCategory : T = Simplicial

(** Joyal's theta category. *)
module Theta = struct

  (** Pasting schemes. *)
  module PS = struct
    type t = PS of t list

    let to_list (PS l) = l

    let point = PS []

    let width p = List.length @@ to_list p

    let eq = (=)

    let compare = compare

    let rec to_string (PS l) = "[" ^ (String.concat "," @@ List.map to_string l) ^ "]"
  end
  module V = PS

  module E = struct
    type t =
      {
        src : PS.t;
        tgt : PS.t;
        simplicial : Simplicial.E.t;
        maps : t list;
      }

    let make src tgt simplicial maps =
      assert (PS.width src = Simplicial.src simplicial);
      assert (PS.width tgt = Simplicial.tgt simplicial);
      let f = Simplicial.ap simplicial in
      let f' = Simplicial.ap @@ Simplicial.interval simplicial in
      let j0 = f 0 in
      List.iteri
        (fun j q ->
           (* TODO: a recursive function *)
           if j >= j0 then
             let p = List.nth (V.to_list src) (f' j - 1) in
             let fj = List.nth maps (j - j0) in
             assert (fj.src = p);
             assert (fj.tgt = q)
        ) (V.to_list src);
      { src; tgt; simplicial; maps }

    let rec to_string f =
      "[" ^ Simplicial.E.to_string f.simplicial ^ "]:[" ^ (String.concat "|" @@ List.map to_string f.maps) ^ "]"

    let rec eq f g =
      Simplicial.E.eq f.simplicial g.simplicial && List.for_all2 eq f.maps g.maps

    let compare (f:t) (g:t) = compare f g
  end

  let src f = f.E.src

  let tgt f = f.E.tgt

  let rec id p = E.make p p (Simplicial.id @@ PS.width p) (List.map id @@ PS.to_list p)

  let comp f g =
    E.make
      (src f)
      (tgt g)
      (Simplicial.comp f.simplicial g.simplicial)
      (failwith "TODO")
end

module ThetaCategory : T = Theta

(** Underlying graph of a category. *)
module Graph (C : T) : Graph.T = struct
  include C
end
