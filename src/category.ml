(** Categories. *)

(** A category. *)
module type T = sig
  (** Objects. *)
  type o

  (** Morphisms. *)
  type t

  (** Source. *)
  val src : t -> o

  (** Target. *)
  val tgt : t -> o

  (** Equality between morphisms. *)
  val eq : t -> t -> bool

  (** Composition of morphisms. *)
  val comp : t -> t -> t

  (** Identity morphism. *)
  val id : o -> t

  val to_string : t -> string

  val compare : t -> t -> int
end

(** Category of a monoid. *)
module Monoid (M : Monoid.T) : T = struct
  type o = unit
  type t = M.t
  let src _ = ()
  let tgt _ = ()
  let eq = M.eq
  let comp = M.mul
  let id () = M.one
  let to_string = M.to_string
  let compare = M.compare
end
