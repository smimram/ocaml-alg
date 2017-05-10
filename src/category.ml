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
module Free (G : Graph.T) : T = struct
  module V = G.V
  module M = Monoid.Free(G.E)
  module E = Alphabet.Prod3(V)(M)(V)
  let src (x,f,y) = x
  let tgt (x,f,y) = y
  let id x = (x,M.one,x)
  let comp (x,f,y) (y',g,z) =
    assert (V.eq y y');
    x,M.mul f g,z
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
