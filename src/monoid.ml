(** Monoids. *)

(** A monoid. *)
module type T = sig
  type t

  (** Equality. *)
  val eq : t -> t -> bool

  (** Multiplication. *)
  val mul : t -> t -> t

  (** Unit. *)
  val one : t

  (** String representation. *)
  val to_string : t -> string

  (** Comparison. *)
  val compare : t -> t -> int
end

(** A commutative monoid. *)
module type Abelian = sig
  type t

  val eq : t -> t -> bool

  (** Addition. *)
  val add : t -> t -> t

  (** Zero. *)
  val zero : t

  (** String representation. *)
  val to_string : t -> string
end

(** The free monoid on a set. *)
module Free (X : Alphabet.T) = struct
  type t = X.t array

  type word = t

  (** Multiplication, i.e. concatenation. *)
  let mul (u:t) (v:t) : t = Array.append u v

  (** Unit of the monoid. *)
  let one : t = [||]

  (** Canonical injection from the alphabet to the monoid. *)
  let inj a : t = [|a|]

  (** Length of a word. *)
  let length (u:t) = Array.length u

  let sub (u:t) o l : t = Array.sub u o l

  let eq u v =
    let ul = length u in
    let vl = length v in
    if ul <> vl then false else
      try
        for i = 0 to ul - 1 do
          if not (X.eq u.(i) v.(i)) then raise Exit
        done;
        true
      with
      | Exit -> false

  let compare u v =
    let ul = length u in
    let vl = length v in
    let ans = ref 0 in
    try
      for i = 0 to min ul vl - 1 do
        let c = X.compare u.(i) v.(i) in
        if c <> 0 then (ans := c; raise Exit)
      done;
      ul - vl
    with
    | Exit -> !ans

  (** Partial equality (equality on subwords). *)
  let peq (u:t) uoff (v:t) voff len =
    try
      for i = 0 to len - 1 do
        if not (X.eq u.(uoff + i) v.(voff + i)) then raise Exit
      done;
      true
    with
    | Exit -> false

  (** String representation of a word. *)
  let to_string (u:t) =
    if eq one u then "ε" else
    let u = Array.to_list u in
    let u = List.map X.to_string u in
    String.concat "" u

  (** Maps from the free monoid to another monoid. *)
  module Map(M : T) = struct
    module E = Map.Make(X)

    type map = M.t E.t

    (** Create a map from an association list. *)
    let of_list l : map = E.of_seq (List.to_seq l)

    (** Apply the map to a letter. *)
    let app (f:map) (x:X.t) =
      E.find x f

    (** Extension of the map to words. *)
    let bind (f:map) (u:t) =
      Array.fold_left (fun y x -> M.mul y (app f x)) M.one u
  end

  let included u v =
    let ul = length u in
    let vl = length v in
    try
      for i = 0 to vl - ul do
        if peq u 0 v i ul then raise Exit
      done;
    false
    with
    | Exit -> true

  (** The leftmost unifier where [u] is on the left and [v] on the right *)
  let unifier ?(i=0) u v =
    let ul = length u in
    let vl = length v in
    let ans = ref None in
    try
      for i = i to ul - 1 do
        if peq u i v 0 (min (ul-i) vl) then
          (
            ans := Some i;
            raise Exit
          )
      done;
      raise Not_found
    with
    | Exit -> (match !ans with Some ans -> ans | None -> assert false)

  (** All unifiers with first on the left. *)
  let ordered_unifiers u v =
    let ans = ref [] in
    let i = ref 0 in
    try
      while true do
        let j = unifier ~i:!i u v in
        ans := j :: !ans;
        i := j+1
      done;
      assert false
    with
    | Not_found -> !ans

  let ordered_unifiers_bicontext u v =
    let lu = length u in
    let lv = length v in
    let l = ordered_unifiers u v in
    List.map (fun i ->
      if i + lv <= lu then
        (one, one),
        (sub u 0 i, sub u (i + lv) (lu - (i + lv)))
      else
        (one, sub v (lu - i) (lv - (lu - i))),
        (sub u 0 i, one)
    ) l

  (** All unifiers, with contexts on the left and on the right. *)
  let unifiers_bicontext u v =
    let l = ordered_unifiers_bicontext u v in
    let l' = ordered_unifiers_bicontext v u in
    let l' = List.map (fun (c,d) -> d,c) l' in
    l@l'

  module Order = struct
    let lexicographic leq u v =
      let ul = length u in
      let vl = length v in
      let ans = ref true in
      try
        for i = 0 to min ul vl - 1 do
          if leq u.(i) v.(i) then
            (if not (X.eq u.(i) v.(i)) then raise Exit)
          else
            (ans := false; raise Exit)
        done;
        ul <= vl
      with
      | Exit -> !ans

    let deglex leq u v =
      let ul = length u in
      let vl = length v in
      if ul < vl then true
      else if ul > vl then false
      else lexicographic leq u v
  end

  (** Anick chains. *)
  module Anick = struct
    (** An Anick chain. *)
    type t = word list

    (** The empty chain. *)
    let empty : t = []

    (** The singleton chain. *)
    let singleton a : t = [inj a]

    (** Singleton chains. *)
    let singletons l = List.map singleton l

    (** Head of the chain. *)
    let hd (l:t) : word = List.hd l

    (** Tail of the chain. *)
    let tl (l:t) : t = List.tl l

    let weq = eq

    let eq c d =
      let weq = eq in
      let rec aux c d =
        match (c,d) with
        | u::c, v::d -> weq u v && aux c d
        | [], [] -> true
        | [], _ -> false
        | _, [] -> false
      in
      aux c d

    let compare c d =
      let wc = compare in
      let rec aux c d =
        match (c,d) with
        | u::c, v::d ->
           let cmp = wc u v in
           if cmp = 0 then aux c d else cmp
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
      in
      aux c d

    (** Compute (n+1)-chains from n-chains. The first argument is the list of
        minimal reducible words. *)
    let extend (l:word list) (c:t) : t list =
      match c with
      | [||]::_ | [] -> assert false
      | u::c ->
         let ul = length u in
         let ans = ref [] in
         let iter_ctx f l =
           let rec aux h = function
             | x::t -> f h x t; aux (x::h) t
             | [] -> ()
           in
           aux [] l
         in
         iter_ctx (fun l1 v l2 ->
           let vl = length v in
           for i = max (ul-vl) 0 to ul - 1 do
             try
               if not (peq u i v 0 (ul-i)) then raise Exit;
               let i = ul - i in
               let v' = sub v i (vl-i) in
               (* Ensure that the suffix is not reducible by any other rule. *)
               let w = mul u v' in
               let f u = if included u w then raise Exit in
               List.iter f l1;
               List.iter f l2;
               (* ...or by the rule itself (excepting at last position). *)
               if included v (sub w 0 (length w - 1)) then raise Exit;
               ans := (v'::u::c) :: !ans
             with
             | Exit -> ()
           done;
         ) l;
         !ans

    (** Add an element to a chain. *)
    let extend l cc = List.concat (List.map (extend l) cc)

    (** Concatenation of the elements of the chain. *)
    let eval (l:t) = List.fold_left mul one (List.rev l)

    (** Dimension of the chain. *)
    let length (l:t) = List.length l
      
    (** String representation. *)
    let to_string (c:t) = "[" ^ String.concat "|" (List.map to_string (List.rev c)) ^ "]"
  end
end
module FreeMonoid (X : Alphabet.T) : T = Free(X)

(** Oriented presentation of a monoid. *)
module Pres (X : Alphabet.T) = struct
  module W = Free(X)

  type t =
    {
      generators : X.t list;
      rules : (W.t * W.t) list;
    }

  let make generators rules =
    { generators; rules }

  (** Orient rules according to a partial order. *)
  let orient leq pres =
    let rules = List.map (fun (u,v) -> if leq v u then u,v else v,u) pres.rules in
    { pres with rules }

  (** Normalize a word. *)
  let rec normalize pres u =
    try
      let v,v' = List.find (fun (v,_) -> W.included v u) pres.rules in
      let i = W.unifier u v in
      let v1 = W.sub u 0 i in
      let v2 = W.sub u (i + W.length v) (W.length u - (i + W.length v)) in
      normalize pres (W.mul v1 (W.mul v v2))
    with
    | Not_found -> u

  let add_rule pres (u,v) =
    { pres with rules = (u,v)::pres.rules }

  (** Reduce a presentation. *)
  let reduce pres =
    let rules = List.map (fun (u,v) -> u, normalize pres v) pres.rules in
    let rec aux h = function
      | (u,v)::t ->
         let f l = List.exists (fun (u',v') -> W.included u' u) l in
         if f h || f t then aux h t else aux ((u,v)::h) t
      | [] -> List.rev h
    in
    let rules = aux [] rules in
    { pres with rules }

  (** Knuth-Bendix completion wrt a total order. *)
  let complete leq pres =
    let pres = orient leq pres in
    let pres = reduce pres in
    let todo = Queue.create () in
    List.iter (fun r -> Queue.add r todo) pres.rules;
    let pres = ref pres in
    (* Add a relation *)
    let rel (u,v) =
      (* Printf.printf "rel: %s\n%!" (A.to_string p); *)
      let u,v = if leq v u then u,v else v,u in
      pres := add_rule !pres (u,v);
      Queue.push (u,v) todo
    in
    while not (Queue.is_empty todo) do
      let u,u' = Queue.pop todo in
      List.iter (fun (v,v') ->
          List.iter (fun ((u1,u2),(v1,v2)) ->
              let s1 = W.mul u1 (W.mul u' u2) in
              let s2 = W.mul v1 (W.mul v' v2) in
              rel (s1,s2)
            ) (W.unifiers_bicontext u v)
        ) !pres.rules
    done;
    !pres

  (** Make a monoid from a convergent presentation. *)
  module Make (P : sig val presentation : t end) : T = struct
    let p = P.presentation

    let nf = normalize p

    type t = W.t

    let mul = W.mul

    let one = W.one

    let to_string = W.to_string

    let compare u v = W.compare (nf u) (nf v)

    let eq u v = W.eq (nf u) (nf v)
  end
end

module Generate (X : Alphabet.T with type t = int) = struct
  module Pres = Pres(X)

  let intset n =
    let rec aux k =
      if k >= n then [] else
        k::(aux (k+1))
    in
    aux 0
end

(* module FreeAbelian(X : Alphabet.T) = struct *)
  (* type t = (X.t * int) list *)

  (* let domain u = List.filter_map (fun (x,n) -> if n > 0 then Some x else None) u *)
(* end *)

(* module FreeAbelianMonoid(X : Alphabet.T) : Abelian = FreeAbelian(X) *)

(** Underlying alphabet of a monoid. *)
module Alphabet (M : T) : Alphabet.T = struct
  include M
end
