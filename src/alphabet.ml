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
module Char : T with type t = char = struct
  type t = char

  let eq c d = (c:char) = (d:char)

  let to_string c = String.make 1 c

  let compare c d = compare (c:char) (d:char)

  let leq c d = (c:char) <= (d:char)

  let geq c d = (c:char) >= (d:char)
end

(** The alphabet of integers. *)
module Int : T with type t = int = struct
  type t = int
  let eq i j = (i:int) = (j:int)

  let to_string = string_of_int

  let compare i j = compare (i:int) (j:int)

  let leq i j = (i:int) <= (j:int)

  let geq i j = (i:int) >= (j:int)
end

(** The alphabet with one element. *)
module Unit : T with type t = unit = struct
  type t = unit

  let eq () () = true

  let compare () () = 0

  let to_string () = "*"
end

(** Product of alphabets. *)
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

(** Triple product of alphabets. *)
module Prod3 (A:T) (B:T) (C:T) : (T with type t = A.t * B.t * C.t) = struct
  type t = A.t * B.t * C.t

  let eq (a,b,c) (a',b',c') =
    A.eq a a' && B.eq b b' && C.eq c c'

  let compare (a,b,c) (a',b',c') =
    let comp = A.compare a a' in
    if comp <> 0 then comp else
      let comp = B.compare b b' in
      if comp <> 0 then comp else
        C.compare c c'

  let to_string (a,b,c) =
    "(" ^ A.to_string a ^ "," ^ B.to_string b ^ "," ^ C.to_string c ^ ")"
end

(** Functions between alphabets. *)
module Map (A:T) (B:T) = struct
  module M = Map.Make(A)

  type t = B.t M.t

  let empty : t = M.empty

  let app (f:t) (x:A.t) = M.find x f
end
