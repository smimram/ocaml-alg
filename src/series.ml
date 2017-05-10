(** Formal series. *)

module Make (K : Field.T) = struct
  (** A formal series. *)
  type t = (K.t Weak.t ref * (int -> K.t))

  let eq a b = failwith "Cannot implement this."

  (** Get a coefficient. *)
  let get (a:t) n =
    let aw, af = a in
    let awl = Weak.length !aw in
    let x = if n < awl then Weak.get !aw n else None in
    match x with
    | Some x -> x
    | None ->
       let x = af n in
       if n >= awl then
         (
           let aw' = Weak.create (n+1) in
           Weak.blit aw' 0 !aw 0 awl;
           aw := aw'
         );
       Weak.set !aw n (Some x);
       x

  let coeff = get

  let to_string a =
    let ans = ref (K.to_string (get a 0)) in
    for i = 1 to 8 do
      let ai = get a i in
      if not (K.eq ai K.zero) then
        ans :=
          !ans
          ^ "+"
          ^ (if K.eq ai K.one then "" else K.to_string ai)
          ^ (if i = 1 then "z" else "z^" ^ string_of_int i)
    done;
    ans := !ans ^ "+...";
    !ans

  let make f : t = (ref (Weak.create 0), f)

  let zero = make (fun _ -> K.zero)

  let one = make (fun n -> if n = 0 then K.one else K.zero)

  let var = make (fun n -> if n = 1 then K.one else K.zero)

  let add a b = make (fun n -> K.add (get a n) (get b n))

  let sub a b = make (fun n -> K.add (get a n) (K.mul (K.neg K.one) (get b n)))

  let mul a b =
    let f n =
      let ans = ref K.zero in
      for i = 0 to n do
        ans := K.add !ans (K.mul (get a i) (get b (n-i)))
      done;
      !ans
    in
    make f

  let rec expn a n =
    assert (n >= 0);
    if n = 0 then one
    else if n = 1 then a else
      mul a (expn a (n-1))

  let hadamard a b = make (fun n -> K.mul (get a n) (get b n))

  let cmul a b = make (fun n -> K.mul a (get b n))

  let neg a = make (fun n -> K.neg (get a n))

  let star a =
    (* TODO *)
    assert (K.eq (get a 0) K.zero);
    (* add the exponents from 0 to n *)
    let rec aux n =
      if n = 0 then expn a 0
      else add (expn a n) (aux (n-1))
    in
    make (fun n -> get (aux n) n)

  let inv a = star (sub one a)

  module Polynomial = Ring.Polynomial(K)

  let polynomial (p : Polynomial.t) =
    make (fun n -> Polynomial.coeff p n)

  module RationalFractions = Field.RationalFractions(K)

  let rational (r : RationalFractions.t) =
    let p, q = r in
    let p = polynomial p in
    let q = polynomial q in
    mul p (inv q)
end

module Field (K : Field.T) : Ring.T = Make(K)
