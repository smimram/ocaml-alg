(** Parity complexes. *)

(** Presentation of a parity complex. *)
module Pres (G : Alphabet.T) = struct
  type generator = G.t
  module C = Set.Make(G)
  type cell = C.t

  (** List of generators along with dimension, source and target. *)
  type t = (generator * (int * cell * cell)) list

  let dim s g =
    let n, _, _ = List.assoc g s in
    n

  let src s g =
    let _, src, _ = List.assoc g s in
    src

  let tgt s g =
    let _, _, tgt = List.assoc g s in
    tgt

  module Cell = struct
    type t = cell
  end
end
