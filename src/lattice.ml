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
  module L = Alphabet.List(X)
  module LL = Alphabet.List(L)

  (** Elements are represented in disjunctive normal form: a sorted list of clauses, each of which is a sorted list of generators without repetitions, and no clause contains another one. This form is canonical, so that two elements are equal in the lattice exactly when they are equal as lists. *)
  type t = LL.t

  let eq = LL.eq

  let compare = LL.compare

  (** Create an element from a dnf. *)
  let make (l : X.t list list) : t =
    let l = List.sort_uniq L.compare @@ List.map (List.sort_uniq X.compare) l in
    let included c d = List.for_all (fun x -> List.exists (X.eq x) d) c in
    List.filter (fun c -> not (List.exists (fun d -> not (L.eq c d) && included d c) l)) l

  (** Smallest element. *)
  let bot = make []

  (** Greatest element. *)
  let top = make [[]]

  (** The element corresponding to a generator. *)
  let generator x = make [[x]]

  (** Supremum. *)
  let sup x y = make (x@y)

  (** Infimum. *)
  let inf x y = make @@ List.map_pairs (fun x' y' -> x'@y') x y

  (** Unique morphism to the booleans extending the given function on generators. *)
  let eval_bool f (x:t) = List.exists (List.for_all f) x

  (** Unique endomorphism extending the given function on generators. *)
  let subst (f:X.t->t) (x:t) =
    List.fold_left (fun a c -> sup a (List.fold_left (fun b g -> inf b (f g)) top c)) bot x

  (** String representation, where ∧ has priority over ∨. *)
  let to_string ?(inf="") ?(sup="∨") (x:t) =
    if eq x bot then "⊥"
    else if eq x top then "⊤"
    else String.concat sup @@ List.map (fun c -> String.concat inf @@ List.map X.to_string c) x
end
