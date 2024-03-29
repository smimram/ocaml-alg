(** Monoids. *)

open Extlib

(** A monoid. *)
module type T = sig
  type t

  (** Equality. *)
  val eq : t -> t -> bool

  (** Multiplication. *)
  val mul : t -> t -> t

  (** Unit. *)
  val one : t

  (** An element expnentiated to a natural number of times. *)
  val pow : t -> int -> t

  (** String representation. *)
  val to_string : t -> string

  (** Comparison. *)
  val compare : t -> t -> int

  (** Whether we are guaranteed to be commutative. *)
  val is_commutative : bool
end

(** A commutative monoid with additive conventions. *)
module type Additive = sig
  type t

  val eq : t -> t -> bool

  val compare : t -> t -> int

  (** Addition. *)
  val add : t -> t -> t

  (** Zero. *)
  val zero : t

  (** String representation. *)
  val to_string : t -> string
end

(** Convert a monoid to additive conventions. *)
module ToAdditive (M : T) : Additive = struct
  let () = assert M.is_commutative
  type t = M.t
  let eq = M.eq
  let compare = M.compare
  let add = M.mul
  let zero = M.one
  let to_string = M.to_string
end

(** Simple implementation of power. *)
let simple_pow one mul u n =
  let ans = ref one in
  for _ = 1 to n do
    ans := mul !ans u
  done;
  !ans

(** The free monoid on a set. *)
module Free (X : Alphabet.T) = struct
  type t = X.t array

  type word = t

  let is_commutative = false

  (** Multiplication, i.e. concatenation. *)
  let mul (u:t) (v:t) : t = Array.append u v

  (** Unit of the monoid. *)
  let one : t = [||]

  let mul_list = List.fold_left mul one

  let pow (u:t) n : t = simple_pow one mul u n

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

  let is_one = eq one

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

  (** Whether a word is included in another. *)
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

  (** Find an offset at which the first word is included in the second. *)
  let inclusion_offset u v =
    let ul = length u in
    let vl = length v in
    let ans = ref None in
    try
      for i = 0 to vl - ul do
        if peq u 0 v i ul then
          (
            ans := Some i;
            raise Exit
          )
      done;
      !ans
    with
    | Exit -> !ans

  (** The leftmost unifier where [u] is on the left and [v] on the
      right. Returns the offset of the position of [v] in [u]. *)
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
  let ordered_unifiers ?(strict=false) u v =
    let ans = ref [] in
    let i = ref (if strict then 1 else 0) in
    try
      while true do
        let j = unifier ~i:!i u v in
        ans := j :: !ans;
        i := j+1
      done;
      assert false
    with
    | Not_found -> !ans

  (** Ordered unifiers with contexts. *)
  let ordered_unifiers_bicontext ?strict u v =
    let lu = length u in
    let lv = length v in
    let l = ordered_unifiers ?strict u v in
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

    (** Compare chains. *)
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

    (** Compare chains. *)
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

(** The free monoid as a monoid. *)
module FreeMonoid (X : Alphabet.T) : T = Free(X)

(** Oriented presentation of a monoid. *)
module Pres (X : Alphabet.T) = struct
  module W = Free(X)

  (** Rules in the presentation. *)
  module Rule = struct
    (** A rewriting rule (with given name, source and target). *)
    type t = string * W.t * W.t

    let name ((n,_,_) : t) = n

    let source ((_,s,_) : t) = s

    let target ((_,_,t) : t) = t

    let eq ((n,u,v):t) ((n',u',v'):t) = String.equal n n' && W.eq u u' && W.eq v v'

    (** Functions to generate rule names (those are used by completion
        procedures to generate rule names). *)
    module Namer = struct
      let none = fun () -> ""

      let simple id =
        let n = ref (-1) in
        fun () ->
          incr n;
          id ^ string_of_int !n
    end
  end

  (** Rewriting steps. *)
  module Step = struct
    type obj = W.t
    
    (** A rewriting step is a rule with a context. *)
    type t = W.t * Rule.t * W.t

    let source ((u,r,w):t) = W.mul_list [u; Rule.source r; w]

    let target ((u,r,w):t) = W.mul_list [u; Rule.target r; w]

    let to_string ((u,r,w):t) =
      let r = Rule.name r in
      let r = if r = "" then "_" else r in
      W.to_string u ^ r ^ W.to_string w

    let eq ((u,r,w):t) ((u',r',w'):t) = W.eq u u' && Rule.eq r r' && W.eq w w'
  end

  (** The underlying abstract rewriting system. *)
  module ARS = ARS.Make(W)(Step)

  (** Rewriting paths. *)
  module Path = ARS.Path

  (** Rewriting zigzags. *)
  module Zigzag = ARS.Zigzag

  (** A presentation. *)
  type t =
    {
      generators : X.t list;
      rules : Rule.t list;
    }

  let make generators rules =
    { generators; rules }

  (** String representation of a presentation. *)
  let to_string pres =
    Printf.sprintf
      "< %s | %s >"
      (List.map X.to_string pres.generators |> String.concat ", ")
      (List.map (fun (n,u,v) ->
           let n = if n = "" then "" else n ^ " : " in
           Printf.sprintf "%s%s -> %s" n (W.to_string u) (W.to_string v)
         ) pres.rules |> String.concat ", ")

  (** Orient rules according to a partial order. *)
  let orient leq pres =
    let rules = List.map (fun (n,u,v) -> if leq v u then n,u,v else n,v,u) pres.rules in
    { pres with rules }

  (** Normalize a word. *)
  let rec normalize pres u =
    try
      let i,r = match List.find_map (fun r -> Option.map (fun i -> i,r) (W.inclusion_offset (Rule.source r) u)) pres.rules with Some ans -> ans | None -> raise Exit in
      let v = Rule.source r in
      let v' = Rule.target r in
      let v1 = W.sub u 0 i in
      let v2 = W.sub u (i + W.length v) (W.length u - (i + W.length v)) in
      normalize pres (W.mul v1 (W.mul v' v2))
    with Exit -> u

  (** Compute a normalization path for a word. *)
  let rec normalization pres u =
    try
      let i,r = match List.find_map (fun r -> Option.map (fun i -> i,r) (W.inclusion_offset (Rule.source r) u)) pres.rules with Some ans -> ans | None -> raise Exit in
      let v = Rule.source r in
      let v' = Rule.target r in
      let v1 = W.sub u 0 i in
      let v2 = W.sub u (i + W.length v) (W.length u - (i + W.length v)) in
      Path.append (Path.step (v1,r,v2)) (normalization pres (W.mul v1 (W.mul v' v2)))
    with Exit -> Path.empty u

  (** Add a rule to a presentation. *)
  let add_rule pres r =
    { pres with rules = r::pres.rules }

  (** Reduce a presentation: normalize right members and remove inclusion
      branchings. *)
  let reduce pres =
    let rules = List.map (fun (n,u,v) -> n, u, normalize pres v) pres.rules in
    let rec aux h = function
      | r::t ->
         let f l = List.exists (fun r' -> W.included (Rule.source r') (Rule.source r)) l in
         if f h || f t then aux h t else aux (r::h) t
      | [] -> List.rev h
    in
    let rules = aux [] rules in
    { pres with rules }

  let global_namer = Rule.Namer.simple "K"

  (** Knuth-Bendix completion wrt a total order. *)
  let complete ?namer leq pres =
    let namer = Option.value namer ~default:global_namer in
    let pres = orient leq pres in
    let todo = Queue.create () in
    List.iter (fun r -> Queue.add r todo) pres.rules;
    let pres = ref pres in
    (* Add a relation *)
    let rel r =
      let u = normalize !pres (Rule.source r) in
      let v = normalize !pres (Rule.target r) in
      if not (W.eq u v) then
        (
          let u,v = if leq v u then u,v else v,u in
          (* Printf.printf "rel: %s -> %s\n%!" (W.to_string u) (W.to_string v); *)
          pres := add_rule !pres (namer (),u,v);
          Queue.push r todo
        )
    in
    while not (Queue.is_empty todo) do
      let r = Queue.pop todo in
      List.iter
        (fun r' ->
           List.iter (fun ((u1,u2),(v1,v2)) ->
               let s1 = W.mul u1 (W.mul (Rule.target r) u2) in
               let s2 = W.mul v1 (W.mul (Rule.target r') v2) in
               rel ("",s1,s2)
             ) (W.unifiers_bicontext (Rule.source r) (Rule.source r'))
        ) !pres.rules
    done;
    !pres

  (** Critical branchings. Returns pairs of rules with context. This might
      return redundant branchings unless the presentation is reduced. *)
  let critical_branchings pres : ((Step.t * Step.t) list) =
    let rec map f = function
      | [] -> []
      | x::l -> (List.map (f x) (x::l))@(map f l)
    in
    let f r s =
      (if Rule.eq r s
       then W.ordered_unifiers_bicontext ~strict:true
       else W.unifiers_bicontext)
        (Rule.source r)
        (Rule.source s)
      |> List.map (fun ((u1,u2),(v1,v2)) -> (u1,r,u2),(v1,s,v2))
    in
    map f pres.rules |> List.concat

  (** Compute the coherence cells. *)
  let coherence pres =
    List.map
      (fun (s1,s2) ->
         Path.append (Path.step s1) (normalization pres (Step.target s1)),
         Path.append (Path.step s2) (normalization pres (Step.target s2))
      ) (critical_branchings pres)

  (** Make a monoid from a convergent presentation. *)
  module Make (P : sig val presentation : t end) : T = struct
    let p = P.presentation

    let nf = normalize p

    type t = W.t

    let is_commutative = false

    let mul = W.mul

    let one = W.one

    let pow = W.pow

    let to_string = W.to_string

    let compare u v = W.compare (nf u) (nf v)

    let eq u v = W.eq (nf u) (nf v)
  end
end

module Generate (X : Alphabet.T with type t = int) = struct
  module Pres = Pres(X)

  module W = Pres.W

  let intset n =
    let rec aux k =
      if k >= n then [] else
        k::(aux (k+1))
    in
    aux 0

  (** Dihedral group of order n. *)
  let dihedral n =
    let r = 0 in
    let s = 1 in
    Pres.make [r;s] [
      "R", W.pow (W.inj r) n, W.one;
      "S", W.pow (W.inj s) 2, W.one;
      "T", [|r;s|], W.mul (W.inj s) (W.pow (W.inj r) (n-1))
    ]
end

(** Multisets over a set, i.e. the free commutative monoid. *)
module Multisets(X : Alphabet.T) = struct
  type t = (X.t * int) list

  let is_commutative = true

  let to_string (u:t) =
    List.fold_left (fun s (x,n) ->
        if n = 0 then s
        else
          let n = if n = 1 then "" else String.superscript (string_of_int n) in
          s ^ X.to_string x ^ n
      ) "" u

  (** Domain of an element. *)
  module Domain = struct
    type t = X.t list

    let empty : t = []

    let union (d:t) (d':t) : t =
      d@d' |> List.sort_uniq X.compare
  end

  let domain (u:t) : Domain.t = List.filter_map (fun (x,n) -> if n > 0 then Some x else None) u

  let occurrences x (u:t) =
    match List.assoc_opt x u with
    | Some n -> n
    | None -> 0

  let one : t = []

  let mul u v : t =
    let d = Domain.union (domain u) (domain v) in
    List.map (fun x -> x, occurrences x u + occurrences x v) d

  let pow (u:t) n : t =
    List.map (fun (x,m) -> x,m*n) u

  let included (u:t) (v:t) =
    List.for_all (fun (x,n) -> n <= occurrences x v) u

  let eq u v = included u v && included v u

  (* Put an element in canonical form. *)
  let normalize u =
    let u = List.filter (fun (x,n) -> n <> 0) u in
    List.sort (fun (x,n) (y,m) ->
        let c = X.compare x y in
        if c <> 0 then c else n - m
      ) u

  let compare (u:t) (v:t) =
    let u = normalize u in
    let v = normalize v in
    let rec aux u v =
      match u, v with
      | (x,n)::u, (y,m)::v ->
        let c = X.compare x y in
        if c <> 0 then c else
          let c = n - m in
          if c <> 0 then c else
            aux u v
      | _::_, [] -> 1
      | [], _::_ -> -1
      | [], [] -> 0
    in
    aux u v

  let inj x : t = [x,1]

  let cmul n u = List.map (fun (x,m) -> x,m*n) u

  module Map(M:T) = struct
    let () = assert M.is_commutative

    module E = Map.Make(X)

    type map = M.t E.t

    let of_list l = E.of_seq (List.to_seq l)

    let app (f:map) (x:X.t) : M.t =
      E.find x f

    let bind (f:map) (u:t) : M.t =
      List.fold_left (fun v (x,n) -> M.mul v (M.pow (app f x) n)) M.one u
  end
end

(** Multisets as a monoid. *)
module MultisetsMonoid(X : Alphabet.T) : T = Multisets(X)

(** Underlying alphabet of a monoid. *)
module Alphabet (M : T) : Alphabet.T = struct
  include M
end
