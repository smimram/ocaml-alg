(** Operations on lattices. *)

open Extlib

(** Bounded lattices. *)
module type BoundedLattice = sig
  type t

  val inf : t -> t -> t

  val sup : t -> t -> t

  val bot : t

  val top : t
end

(** The free bounded distributive lattice on a set of generators. *)
module FreeBoundedDistributiveLattice (X : Alphabet.T) = struct

  (* Elements are represented in disjunctive normal form (sorted, without repetitions). *)
  module E = struct
    module L = Alphabet.List(X)
    module LL = Alphabet.List(L)
    include LL

    (** Create an element from a dnf. *)
    let make (l : X.t list list) : t =
      List.sort_uniq L.compare @@ List.map (List.sort_uniq X.compare) l
  end

  type t = E.t

  let bot = E.make []

  let top = E.make [[]]

  let sup x y = E.make (x@y)

  let inf x y = E.make @@ List.map_pairs (fun x' y' -> x'@y') x y
end
