(** Groups. *)

(** A group. *)
module type T = sig
  include Monoid.T

  val inv : t -> t
end

(** An abelian group (with additive conventions). *)
module type Additive = sig
  include Monoid.Additive

  val neg : t -> t
end
