(** Modules. *)

(** A left module. *)
module type Left = sig
  (** An element of the module. *)
  type t

  (** An element of the ring. *)
  type r

  module Ring : sig
    include Ring.T with type t := r
  end

  include Group.Abelian with type t := t

  (** Left action of the ring. *)
  val cmul : r -> t -> t
end

(** A right module. *)
module type Right = sig
  include Left

  (** Right action of the ring. *)
  val cmul : t -> r -> t
end

(*
(** A bimodule. *)
module type Bi = sig
  include Left
  include Right
end
*)

(** By default, by a module, we mean a left module. *)
module type T = Left

module FreeLeft (R : Ring.T) (X : Alphabet.T) = struct
  module Ring = R

  module E = struct
    include Map.Make(X)

    let add x a p =
      if R.eq R.zero a then
        remove x p
      else
        add x a p
  end

  (** An element of the ring. *)
  type r = R.t

  (** An element of the module. *)
  type t = r E.t
  (* type element = t *)

  let zero : t = E.empty

  let cinj (a:R.t) (x:X.t) : t = E.add x a zero

  let inj x = cinj R.one x

  (** Coefficient of an element. *)
  let coeff (p:t) (x:X.t) =
    try
      E.find x p
    with
    | Not_found -> R.zero

  let included x y =
    E.for_all (fun u a -> coeff y u = a) (x:t)

  let eq x y =
    included x y && included y x

  let add_monomial (p:t) (a:r) (x:X.t) : t =
    let a = R.add a (coeff p x) in
    E.add x a p

  let add (p:t) (q:t) : t =
    E.fold (fun x a p -> add_monomial p a x) q p

  let cmul a (x:t) : t =
    E.map (R.mul a) x

  let neg (x:t) =
    cmul (R.neg R.one) x

  let sub x y =
    add x (neg y)

  let to_string (x:t) =
    if eq zero x then "0" else
    let ans = ref "" in
    E.iter (fun u a ->
      if !ans <> "" then ans := !ans ^ "+";
      let a =
        if R.eq a R.one then "" else
          "(" ^ R.to_string a ^ ")" ^ "*"
      in
      ans := !ans ^ a ^ X.to_string u
    ) x;
    !ans

  (** Map a linear function. *)
  let map f (p:t) =
    E.fold (fun (x:X.t) a q -> add q (cmul a (f x))) p zero

  let iter f p =
    E.iter (fun (x:X.t) (a:R.t) -> f a x) p

  (** Morphisms between free modules. *)
  module Map = struct
    module E = Map.Make(X)

    type map = t E.t

    let set (f:map) (x:X.t) (p:t) : map =
      E.add x p f

    let app (f:map) (x:X.t) =
      try
        E.find x f
      with
      | Not_found -> zero

    let bind f (p:t) =
      E.fold (fun (x:X.t) a q -> add q (cmul a (app f x))) p zero

    let zero : map = E.empty

    let to_string (f:map) =
      E.fold (fun (x:X.t) (p:t) s -> s ^ (X.to_string x) ^ " -> " ^ (to_string p) ^ "\n") f ""

    type t = map
  end

  (** A presentation of a free module, i.e. a basis. *)
  module Presentation = struct
    (** A presentation. *)
    type pres = X.t array

    type t = pres

    (** Create a presentation. *)
    let make gen = (gen : t)

    (** Dimension of a presentation. *)
    let dim (pres : t) = Array.length pres

    let presentation_to_string pres =
      let ans = ref "" in
      Array.iter (fun x ->
        ans := !ans ^ (if !ans = "" then "" else " ") ^ X.to_string x
      ) pres;
      !ans

    (** Linear maps between presentations. *)
    module Map = struct
      module M = Matrix.Make(R)
      module L = M.Labeled(X)

      (** A linear map (encoded as a matrix). *)
      type map = L.t
      type t = map

      (** Apply a morphism to an element. *)
      let app (f:t) p =
        map (fun x ->
          let ans = ref zero in
          L.iter_tgt (fun y ->
            ans := add !ans (cinj (L.get f x y) y)
          ) f;
          !ans
        ) p
        
      (** The zero morphism. *)
      let zero src tgt : t = L.zero src tgt

      (** Create from a map with given source and target. *)
      let of_map f src tgt =
        let ans = zero src tgt in
        Array.iter (fun x ->
          let p = Map.app f x in
          iter (fun a y -> L.set ans x y (R.add (L.get ans x y) a)) p
        ) src;
        ans

      (** Convert to a map. *)
      let to_map (f:t) =
        let ans = ref Map.zero in
        L.iter (fun x y ->
          ans := Map.set !ans x (add (Map.app !ans x) (cinj (L.get f x y) y))
        ) f;
        !ans

      (** Rank of a map. *)
      let rank : t -> int = L.rank

      (** Nullity of a map. *)
      let nullity : t -> int = L.nullity

      (* TODO: improve this *)
      let to_string f =
        Map.to_string (to_map f)
    end

    (** Iterate a function on the generators of a module. *)
    let iter f (pres:t) = Array.iter f pres

    (** Chain complexes between free modules. *)
    module Complex = struct
      (** A chain complex. *)
      type t =
        {
          modules : pres array;
          d : Map.t array
        }

      let modules c = c.modules

      let maps c = c.d

      (** Length of a chain complex. *)
      let length c = Array.length c.d

      (** Create a chain complex. *)
      let make modules d =
        assert (Array.length modules = Array.length d + 1);
        { modules; d }

      (** Ensure that a chain complex satsifies d^2=0. *)
      let valid c =
        try
          for i = 1 to length c - 1 do
            iter (fun x ->
              let y = Map.app c.d.(i-1) (Map.app c.d.(i) (inj x)) in
              if not (eq zero y) then
                (
                  Printf.printf "invalid: %d\n%!" i;
                  Printf.printf "d(%s) = %s\n%!" (X.to_string x) (to_string (Map.app c.d.(i) (inj x)));
                  Printf.printf "d^2(%s) = %s\n%!" (X.to_string x) (to_string (Map.app c.d.(i-1) (Map.app c.d.(i) (inj x))));
                  raise Exit
                )
            ) c.modules.(i+1)
          done;
          true
        with
        | Exit -> false

      (** String representation. *)
      let to_string c =
        let ans = ref "" in
        (* ans := !ans ^ "C" ^ string_of_int (length c) ^ ": " ^ presentation_to_string c.modules.(length c) ^ "\n\n"; *)
        for i = length c - 1 downto 0 do
          ans := !ans ^ "d" ^ string_of_int i ^ ":\n" ^ Map.to_string c.d.(i) ^ "\n";
          (* ans := !ans ^ "C" ^ string_of_int i ^ ": " ^ presentation_to_string c.modules.(i) ^ "\n\n" *)
        done;
        !ans

      (** Compute the homology of the complex. *)
      let homology c =
        Array.init (length c) (fun i ->
          let ker = if i = 0 then dim c.modules.(0) else Map.nullity c.d.(i-1) in
          let im = Map.rank c.d.(i) in
          ker - im
        )
    end

    (** String representation. *)
    let to_string = presentation_to_string
  end
end
module FreeLeftModule (R : Ring.T) (X : Alphabet.T) = (FreeLeft(R)(X) : Left)
module Free (R : Ring.T) (X : Alphabet.T) = FreeLeft(R)(X)

module FreeRight (R : Ring.T) (X : Alphabet.T) = struct
  include FreeLeft(Ring.Op(R))(X)

  let cinj x a = cinj a x
    
  let cmul x a = cmul a x

  let to_string (x:t) =
    let ans = ref "" in
    E.iter (fun u a ->
      if not (R.eq a R.zero) then
        (
          if !ans <> "" then ans := !ans ^ "+";
          let a =
            if R.eq a R.one then "" else
              "*" ^ "(" ^ R.to_string a ^ ")"
          in
          ans := !ans ^ X.to_string u ^ a
        )
    ) x;
    !ans
end
module FreeRightModule (R : Ring.T) (X : Alphabet.T) = (FreeRight(R)(X) : Right)
