(** Simplicial sets. *)

(** A simplicial set. *)
module type T = sig
  (** A simplex. *)
  type t

  val to_string : t -> string

  (** Dimension of a simplex. *)
  val dim : t -> int

  (** Faces of a simplex. *)
  val face : t -> int -> int
end

module Presentation (X : Alphabet.T) = struct
end
