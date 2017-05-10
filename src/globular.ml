(** Globular sets. *)

(** A globular set. *)
module type T = sig
  (** A cell. *)
  type t

  val to_string : t -> string

  (** Dimension. *)
  val dim : t -> int

  (** Source. *)
  val src : t -> t

  (** Target. *)
  val tgt : t -> t
end

(** The terminal globular set. *)
module Terminal :  T = struct
  type t = int

  let to_string = string_of_int

  let dim n = n

  let src n =
    assert (n > 0);
    n - 1

  let tgt n =
    assert (n > 0);
    n - 1
end

(** Presented globular set. *)
module Presentation (X : Alphabet.T) = struct
  module E = Map.Make(X)

  (** A globular set. *)
  (* coded as element, (source,target), the source and target being the
     element for 0-cells *)
  type t = (int * X.t * X.t) E.t

  (** The empty globular set. *)
  let empty : t = E.empty

  let mem (s:t) (g:X.t) = E.mem g s

  (** Dimension of a generator. *)
  let dim (s:t) (g:X.t) =
    let n,_,_ = E.find g s in
    n

  (** Source of a cell. *)
  let src (s:t) (g:X.t) =
    assert (dim s g > 0);
    let n,src,_ = E.find g s in
    assert (n > 0);
    src

  (** Target of a cell. *)
  let tgt (s:t) (g:X.t) =
    let n,_,tgt = E.find g s in
    assert (n > 0);
    tgt

  (** Add a 0-cell. *)
  let add0 (s:t) (g:X.t) : t =
    E.add g (0,g,g) s

  (** Add an n-cell. *)
  let add s (g:X.t) src tgt : t =
    assert (mem s src);
    assert (mem s tgt);
    let n = dim s src + 1 in
    assert (dim s tgt + 1 = n);
    E.add g (n,src,tgt) s

  module Globular (P : sig val presentation : t end) : T = struct
    type t = X.t

    let to_string = X.to_string

    let dim = dim P.presentation

    let src = src P.presentation

    let tgt = tgt P.presentation
  end
end

(** A globular theory, the typical example being weak omega-categories. *)
module Theory = struct
  module Var = struct
    type t = int

    let fresh =
      let n = ref (-1) in
      fun () ->
      incr n; (!n : t)

    let to_string (x:t) =
      "x" ^ string_of_int x
  end

  module Cons = struct
    type t = int
           
    let fresh =
      let n = ref (-1) in
      fun () ->
      incr n; (!n : t)

    let to_string (x:t) =
      "f" ^ string_of_int x
  end

  type term =
    | Var of Var.t
    | Cons of Cons.t * context
  (** A context contains terms with their source and target. *)
  and context = (term * (term * term)) list
end
