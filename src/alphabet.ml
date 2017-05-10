(** Alphabets. *)

(** An alphabet is simply another name for a set, but the module [Set] already
    exists. *)

(** An alphabet. *)
module type T = sig
  (** A letter. *)
  type t

  val eq : t -> t -> bool

  val to_string : t -> string

  val compare : t -> t -> int
end

(** The alphabet of characters. *)
module Char = struct
  type t = char

  let eq c d = (c:char) = (d:char)

  let to_string c = String.make 1 c

  let compare c d = compare (c:char) (d:char)

  let leq c d = (c:char) <= (d:char)

  let geq c d = (c:char) >= (d:char)
end
module CharAlphabet = (Char : T)

module Int = struct
  type t = int
  let eq i j = (i:int) = (j:int)

  let to_string = string_of_int

  let compare i j = compare (i:int) (j:int)

  let leq i j = (i:int) <= (j:int)

  let geq i j = (i:int) >= (j:int)
end

module Prod (A:T) (B:T) : (T with type t = A.t * B.t) = struct
  type t = A.t * B.t

  let eq (a,b) (a',b') =
    A.eq a a' && B.eq b b'

  let compare (a,b) (a',b') =
    let c = A.compare a a' in
    if c = 0 then B.compare b b'
    else c

  let to_string (a,b) =
    "(" ^ A.to_string a ^ "," ^ B.to_string b ^ ")"
end
