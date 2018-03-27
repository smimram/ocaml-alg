(** Categories. *)

(** A category. *)
module type T = sig
  include Graph.T

  (** Composition of morphisms. *)
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

(** Underlying graph of a category. *)
module Graph (C : T) : Graph.T = struct
  include C
end
