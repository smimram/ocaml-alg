(** Fields. *)

(** A field. *)
module type T = sig
  include Ring.T

  (** Inverse. *)
  val inv : t -> t
end

(** The field with two elements. *)
module Bool : (T with type t = bool) = struct
  include Ring.Bool

  let inv (x:t) = x
end

(** The field of floats. *)
module Float : (T with type t = float) = struct
  include Ring.Float

  let inv (x:t) : t = 1. /. x
end

(** The "field" of integers. *)
module Int = struct
  include Ring.Int

  let inv x =
    if eq one x then one
    else if eq (neg one) x then (neg one)
    else
      failwith ("Cannot invert "^string_of_int x^".")
end

(** Field of fractions over an euclidean domain. *)
module Fractions (R : Ring.Euclidean) = struct
  type t = R.t * R.t

  (** Greatest common divisor. *)
  let rec gcd a b =
    let rem a b = snd (R.div a b) in
    if R.eq R.zero b then a else gcd b (rem a b)

  let canonize ((a,b):t) : t =
    let d = gcd a b in
    let a,_ = R.div a d in
    let b,_ = R.div a d in
    (a,b)

  let eq ((a,b):t) ((c,d):t) =
    let (a,b) = canonize (a,b) in
    let (c,d) = canonize (c,d) in
    (R.eq a c && R.eq b d) || (R.eq (R.neg a) c && R.eq (R.neg b) d)

  let add ((a,b):t) ((c,d):t) : t = (R.add (R.mul a d) (R.mul b c), R.mul c d)

  let zero : t = (R.zero, R.one)

  let neg ((a,b):t) : t = (R.neg a, b)

  let mul ((a,b):t) ((c,d):t) : t = (R.mul a c, R.mul c d)

  let one : t = (R.one, R.one)

  let inv ((a,b):t) : t = (b,a)

  let to_string ((a,b):t) =
    "(" ^ R.to_string a ^ ")/(" ^ R.to_string b ^ ")"
end

module FractionsField (R : Ring.Euclidean) : T = Fractions(R)

(** Field of rational fractions. *)
module RationalFractions (F : T) = struct
   (** Ring of polynomials over a field. *)
  module Polynomial (F : T) : Ring.Euclidean with type t = Ring.Polynomial(F).t = struct
    include Ring.Polynomial(F)

    (** Euclidean division. *)
    let div a b =
      let db = degree b in
      assert (db >= 0);
      let q = ref zero in
      let r = ref a in
      let dr = ref (degree !r) in
      while !dr >= db do
        (* quotient of leading monomials of r and b *)
        let t = monomial (F.mul !r.(!dr) (F.inv b.(db))) (!dr - db) in
        q := add !q t;
        r := sub !r (mul t b);
        dr := degree !r
      done;
      !q, !r    
  end

  include Fractions(Polynomial(F))
end

module RationalFractionsField (F : T) : T = RationalFractions(F)

(** Underlying ring of a field. *)
module Ring (F : T) : Ring.T = struct
  include F
end
