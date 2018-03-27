(** Hypergraphs. *)

module type T = sig
  (** Vertices. *)
  module V : Alphabet.T

  (** Edges. *)
  module E : Alphabet.T

  (** Source. *)
  val src : E.t -> V.t list

  (** Target. *)
  val tgt : E.t -> V.t list
end

module Full (V : Alphabet.T) (E : Alphabet.T) = struct
  module V = struct
    type t = { label : V.t }

    let label x = x.label

    let eq x y = x == y

    let compare x y = V.compare (label x) (label y)

    let to_string x = V.to_string (label x)
  end

  module VL = Alphabet.List(V)

  module E = struct
    (* Alphabet.Prod(E)(Alphabet.Prod(Alphabet.List(V))(Alphabet.List(V))) *)

    type t = { label : E.t; src : V.t list; tgt : V.t list }

    let eq f g = f == g

    let compare f g = compare f g

    let to_string f = E.to_string f.label ^ " : " ^ VL.to_string f.src ^ " -> " ^ VL.to_string f.tgt

    let label f = f.label

    let src f = f.src

    let tgt f = f.tgt

    let make l s t = { label = l; src = s; tgt = t}
  end

  let src = E.src

  let tgt = E.tgt
end

(** Presentation of an hypergraph. *)
module Pres (V : Alphabet.T) (E : Alphabet.T) = struct
  include Full(V)(E)

  type t =
    {
      vertices : V.t list;
      edges : E.t list;
    }

  let vertices g = g.vertices

  let edges g = g.edges

  let empty = { vertices = []; edges = [] }

  let add_vertex g x = { g with vertices = x::(vertices g) }

  let add_edge g e = { g with edges = e::(edges g) }

  let vertex_pred g v =
    assert (List.memq v (vertices g));
    List.filter (fun e -> List.memq v (tgt e)) (edges g)

  let vertex_succ g v =
    assert (List.memq v (vertices g));
    List.filter (fun e -> List.memq v (src e)) (edges g)

  let edge_pred g e = src e

  let edge_succ g e = tgt e
end

module Map (V : Alphabet.T) (E : Alphabet.T) = struct
  module MV = Map.Make(V)
  module ME = Map.Make(E)
end
