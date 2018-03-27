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

module Full (V : Alphabet.T) (E : Alphabet.T) = struct
  module V = V

  module E = struct
    include Alphabet.Prod(E)(Alphabet.Prod(V)(V))
    let to_string (f,(x,y)) = E.to_string f
  end

  let src (f,(x,y)) = x

  let tgt (f,(x,y)) = y
end

(** Presentation of a graph. *)
module Pres (V : Alphabet.T) (E : Alphabet.T) = struct
  module Graph = Full(V)(E)

  type t =
    {
      vertices : V.t list;
      edges : Graph.E.t list;
    }

  let empty = { vertices = []; edges = [] }

  let add_vertex p x = { p with vertices = x::p.vertices }

  let add_edge p f ((x:V.t),(y:V.t)) = { p with edges = (f,(x,y))::p.edges }

  let edge p f : Graph.E.t =
    let x,y = List.assoc f p.edges in
    f,(x,y)

  let has_vertex p x = List.exists (fun y -> V.eq x y) p.vertices

  let to_string g =
    let vertices = List.map V.to_string g.vertices in
    let vertices = String.concat " , " vertices in
    let edges = List.map (fun (f,(x,y)) -> E.to_string f ^ ":" ^ V.to_string x ^ "->" ^ V.to_string y) g.edges in
    let edges = String.concat " , " edges in
    vertices ^ "\n" ^ edges
end
