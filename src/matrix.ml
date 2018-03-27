(** Matrices. *)

(** Matrices over a ring. *)
module Make (R:Ring.T) = struct
  (** A matrix. *)
  type t = R.t array array

  type matrix = t

  (** Zero matrix. *)
  let zero r c : t = Array.init r (fun _ -> Array.make c R.zero)

  (** Initialize a matrix. *)
  let init r c f : t =
    Array.init r (fun i -> Array.init c (fun j -> f i j))

  (** Number of rows. *)
  let rows (m : t) = Array.length m

  (** Number of columns. *)
  let cols (m : t) = Array.length m.(0)

  (** Coefficient. *)
  let get (m:t) i j = m.(i).(j)

  (** String representation. *)
  let to_string m =
    let ans = ref "" in
    for i = 0 to rows m - 1 do
      for j = 0 to cols m - 1 do
        if j <> 0 then ans := !ans ^ " ";
        ans := !ans ^ R.to_string m.(i).(j)
      done;
      ans := !ans ^ "\n"
    done;
    !ans

  (** Operations on rows. *)
  module Row = struct
    (** A a row null? *)
    let is_zero m i =
      Array.for_all (R.eq R.zero) m.(i)

    (** Replace a row by another one. *)
    let replace m i mi : t =
      Array.init (rows m) (fun k -> if k = i then mi else m.(k))

    (** Exchange two rows. *)
    let exchange m i j : t =
      Array.init (rows m) (fun k -> if k = i then m.(j) else if k = j then m.(i) else m.(k))

    (** Multiply a row by a coefficent. *)
    let mult m q i =
      assert (not (R.eq R.zero q));
      let mi = Array.init (cols m) (fun k -> R.mul q m.(i).(k)) in
      replace m i mi

    (** Add to a row [q] times another row. *)
    let madd m i q j =
      assert (i <> j);
      let mi = Array.init (cols m) (fun k -> R.add m.(i).(k) (R.mul q m.(j).(k))) in
      replace m i mi
  end

  (** Put a matrix in row echelon form. *)
  let row_echelon m =
    let m = ref m in
    let ip = ref 0 in
    let cols = if rows !m = 0 then 0 else cols !m in
    try
      for j = 0 to cols - 1 do
        if !ip >= rows !m then raise Exit;
        if R.eq R.zero !m.(!ip).(j) then
          (
            try
              for i = !ip + 1 to rows !m - 1 do
                if not (R.eq R.zero !m.(i).(j)) then (m := Row.exchange !m !ip i; raise Exit)
              done
            with
            | Exit -> ()
          );
        let a = !m.(!ip).(j) in
        if not (R.eq R.zero a) then
          (
            for i = !ip + 1 to rows !m - 1 do
              let b = !m.(i).(j) in
              if not (R.eq R.zero b) then
                (
                  m := Row.mult !m (R.neg a) i;
                  m := Row.madd !m i b !ip
                )
            done;
            incr ip
          )
      done;
      !m
    with
    | Exit -> !m

  (** Dimension of the image. *)
  let rank m =
    let m = row_echelon m in
    let n = ref 0 in
    try
      (*
      if rows m = 0 then raise Exit; (* Avoid a problem with undefined cols below. *)
      for i = 0 to min (rows m) (cols m) - 1 do
        if R.eq R.zero m.(i).(i) then raise Exit else incr n
      done;
      *)
      for i = 0 to rows m - 1 do
        if Row.is_zero m i then raise Exit;
        incr n
      done;
      !n
    with
    | Exit -> !n

  (** Dimension of the kernel. *)
  let nullity m =
    rows m - rank m

  (** Matrices with labeled basis elements. *)
  module Labeled (X:Alphabet.T) = struct
    module L = struct
      include Map.Make(X)

      let find : X.t -> 'a t -> 'a = find

      let iter : (X.t -> 'a -> unit) -> 'a t -> unit = iter

      let of_array (a:X.t array) =
        let ans = ref empty in
        for i = 0 to Array.length a - 1 do
          ans := add a.(i) i !ans
        done;
        !ans
    end

    type map = int L.t

    (** A matrix with labeled basis elements. *)
    type t = map * map * matrix

    (** Underlying (non-labeled) matrix. *)
    let matrix ((_,_,m):t) = m

    let zero rows cols : t =
      let m = zero (Array.length rows) (Array.length cols) in
      L.of_array rows, L.of_array cols, m

    (** Set coefficient. *)
    let set ((r,c,m):t) i j x =
      let i = L.find i r in
      let j = L.find j c in
      m.(i).(j) <- x

    (** Coefficient. *)
    let get ((r,c,m):t) i j =
      let i = L.find i r in
      let j = L.find j c in
      m.(i).(j)

    (** Rank. *)
    let rank ((r,c,m):t) = rank m

    let nullity ((r,c,m):t) = nullity m

    (** Iterate over source basis. *)
    let iter_src f ((r,c,m):t) =
      L.iter (fun x _ -> f x) r

    (** Iterate over target basis. *)
    let iter_tgt f ((r,c,m):t) =
      L.iter (fun x _ -> f x) c

    (** Iterate over source and target basis. *)
    let iter f (m:t) =
      iter_src (fun x -> iter_tgt (fun y -> f x y) m) m
  end
end

(** Functors between different rings. *)
module Functor (R:Ring.T) (R':Ring.T) = struct
  module M = Make(R)
  module M' = Make(R')

  let map f (m:M.t) : M'.t =
    let r = M.rows m in
    let c = if r = 0 then 0 else M.cols m in
    M'.init r c (fun i j -> f (M.get m i j))

  module Labeled (X:Alphabet.T) (X':Alphabet.T) = struct
    module L = M.Labeled(X)
    module L' = M'.Labeled(X')

    let map s t f (m:L.t) : L'.t =
      let r,c,m = m in
      let lmap (f:X.t->X'.t) (l:L.map) : L'.map = L.L.fold (fun x i l' -> L'.L.add (f x) i l') l L'.L.empty in
      let r = lmap s r in
      let c = lmap t c in
      let m = map f m in
      r,c,m
  end
end
