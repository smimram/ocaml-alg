(** Polygraphs. *)

(** A polygraph. *)
module type T = sig
  (** Generators. *)
  module G : Alphabet.T

  (** Dimension of a generator. *)
  val dim : G.t -> int

  (** A cell. *)
  type cell

  (** Source of a polygraph. *)
  val source : G.t -> cell

  (** Target of a polygraph. *)
  val target : G.t -> cell
end
