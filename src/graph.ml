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

(* module Presentation (V : Alphabet.T) = struct *)
  (* type t = *)
    (* { *)
      (* vertices : V.t list; *)
      (* edges : (E.t * (V.t * V.t)) list; *)
    (* } *)

  (* let empty = { vertices = []; edges = [] } *)

  (* let add_vertex p x = { p with vertices = x::p.vertices } *)

  (* let add_edge p f (x,y) = { p with edges = (f,(x,y))::p.edges } *)
(* end *)
