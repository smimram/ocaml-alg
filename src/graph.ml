(** Graphs. *)

(** A graph. *)
module type T = sig
  (** Vertices. *)
  module V : Alphabet.T

  (** Edges. *)
  module E : Alphabet.T

  (** Source. *)
  val src : E.t -> V.t

  (** Target. *)
  val tgt : E.t -> V.t
end

(** The terminal graph. *)
module Terminal : T = struct
  module V = Alphabet.Unit
  module E = Alphabet.Unit
  let src () = ()
  let tgt () = ()
end

(** Presentation of a graph. *)
module Presentation (V : Alphabet.T) (E : Alphabet.T) = struct
  module EM = Alphabet.Map(E)(Alphabet.Prod(V)(V))

  type t =
    {
      vertices : V.t list;
      edges : EM.t;
    }

  let empty = { vertices = []; edges = EM.empty }

  let add_vertex p x = { p with vertices = x::p.vertices }

  let add_edge p f (x,y) = { p with edges = EM.add p.edges f (x,y) }
end
