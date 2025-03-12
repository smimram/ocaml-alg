(** Precubical sets. *)

(** Direction for taking faces. *)
type dir = [`Src | `Tgt]

(** A Precubical set. *)
module type T = sig
  (** A cube. *)
  type t

  val to_string : t -> string

  (** Dimension of a simplex. *)
  val dim : t -> int

  (** Faces of a cube. *)
  val face : t -> int -> dir -> t
end

(** Presentation of a precubical set. *)
module Pres (X : Alphabet.T) = struct
  (** Relations. *)
  module Relation = struct
    (** A relation formally identifies two faces of cubes. *)
    type t = (X.t * int * dir) * (X.t * int * dir)
  end

  (** Presentation of a precubical sets. *)
  type t =
    {
      generators : X.t list; (** generating cubes *)
      relations : Relation.t list; (** face identifications *)
    }
end
