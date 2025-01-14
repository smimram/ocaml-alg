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

(** The quaternion group (Q8). *)
module Quaternion = struct
  type gen = E | I | J | K

  (* true means negated *)
  type t = bool * gen

  let to_string ((s,x):t) =
    let s = if s then "-" else "" in
    let x = match x with E -> "1" | I -> "i" | J -> "j" | K -> "k" in
    s ^ x

  let one : t = false, E

  let i : t = false, I
  let j : t = false, J
  let k : t = false, K

  let is_negative (s,_) = s
  
  let neg ((s,x):t) : t = not s, x

  (** All the elements of the group. *)
  let elements = [one;i;j;k;neg one;neg i;neg j;neg k]

  let may_neg b x = if b then neg x else x

  let eq (x:t) (y:t) = x = y

  let compare (x:t) (y:t) = compare x y

  let mul ((s,x):t) ((t,y):t) =
    let mul_gen x y =
      match x, y with
      | E, y -> false, y
      | x, E -> false, x
      | I, J -> false, K
      | J, K -> false, I
      | K, I -> false, J
      | J, I -> true, K
      | K, J -> true, I
      | I, K -> true, J
      | I, I
      | J, J
      | K, K -> true, E
    in
    mul_gen x y |> may_neg s |> may_neg t

  let is_commutative = false

  let pow x n = Monoid.simple_pow one mul x n

  let inv x = neg x
end
module QuaternionGroup = (Quaternion : T)
