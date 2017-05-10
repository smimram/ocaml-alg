(** Groups. *)

(** A group. *)
module type T = sig
  include Monoid.T

  val inv : t -> t
end

(** An abelian group (with additive conventions). *)
module type Abelian = sig
  include Monoid.Abelian

  val neg : t -> t
end
