(** Rings. *)

(** A ring. *)
module type T = sig
  type t

  val eq : t -> t -> bool

  val add : t -> t -> t

  val zero : t

  val neg : t -> t

  val mul : t -> t -> t

  val one : t

  val to_string : t -> string
end

(** An euclidean domain. *)
module type Euclidean = sig
  include T

  val div : t -> t -> (t * t)
end

module Bool : (T with type t = bool) = struct
  type t = bool

  let eq x y = (x:bool) = (y:bool)

  let add x y = (x || y) && not (x && y)

  let zero = false

  let neg x = x

  let mul x y = x && y

  let one = true

  let to_string x =
    if x then "T" else "F"
end

module Int : (T with type t = int) = struct
  type t = int

  let eq x y = (x:int) = (y:int)

  let add = ( + )

  let zero = 0

  let neg x = - x

  let mul = ( * )

  let one = 1

  let to_string = string_of_int
end

module Float : (T with type t = float) = struct
  type t = float

  let eq (x:t) (y:t) = x = y

  let add = ( +. )

  let zero = 0.

  let neg x = -. x

  let mul = ( *. )

  let one = 1.

  let to_string = string_of_float
end

(** Polynomial ring over a ring. *)
module Polynomial (R : T) = struct
  (** A polynomial. *)
  type t = R.t array

  let length (p:t) = Array.length p

  (** Degree of a polynomial. *)
  let degree (p:t) =
    let ans = ref 0 in
    try
      for i = length p - 1 downto 0 do
        if not (R.eq R.zero p.(i)) then
          (
            ans := i;
            raise Exit
          )
      done;
      min_int
    with
    | Exit -> !ans

  let eq (p:t) (q:t) =
    let dp = degree p in
    let dq = degree q in
    try
      if dp <> dq then raise Exit;
      for i = 0 to dp - 1 do
        if not (R.eq p.(i) q.(i)) then raise Exit
        done;
        true
    with
    | Exit -> false

  let compact p : t =
    Array.init (degree p) (fun i -> p.(i))

  let coeff p i =
    if i < length p then p.(i) else R.zero

  let init n f : t =
    Array.init n f

  let rec add p q =
    let pl = length p in
    let ql = length q in
    if pl > ql then add q p else
      init ql (fun i -> if i < pl then R.add p.(i) q.(i) else q.(i))

  let zero = [||]

  let cmul a (p:t) : t =
    Array.map (R.mul a) p

  let neg p = cmul (R.neg R.one) p

  let sub p q =
    add p (neg q)

  let mul p q =
    init (degree p + degree q) (fun n ->
      let ans = ref R.zero in
      for i = 0 to n do
        ans := R.add !ans (R.mul (coeff p i) (coeff q (n-i)))
      done;
      !ans
    )

  let one = [|R.one|]

  let to_string (p:t) =
    let ans = ref "" in
    for i = 0 to length p - 1 do
      ans := !ans
      ^ (if i <> 0 then "+" else "")
      ^ (if R.eq R.zero p.(i) then "" else R.to_string p.(i) ^ if i = 0 then "" else ("X^" ^ string_of_int i))
    done;
    !ans

  let monomial c n =
    let ans = Array.make (n+1) R.zero in
    ans.(n) <- c;
    ans
end
module PolynomialRing (R : T) = (Polynomial(R) : T)

(** Opposite ring. *)
module Op (R : T) : (T with type t = R.t) = struct
  include R

  let mul x y = mul y x
end
