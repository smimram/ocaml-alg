(** Term rewriting systems. *)

open Extlib

(** Operations. *)
module Op = struct
  (** An operation. *)
  type t =
    {
      weight : int;
      name : string;
      arity : int;
      to_string : string list -> string;
    }

  (** Name of an operation. *)
  let name f = f.name

  (** Weight of an operation. *)
  let weight f = f.weight

  (** Arity of an operation. *)
  let arity f = f.arity

  (** Create an operation with given name and arity. *)
  let make ?to_string ?(weight=0) name arity : t =
    let to_string =
      match to_string with
      | Some to_string -> to_string
      | None -> fun a -> name ^ "(" ^ String.concat "," a ^ ")"
    in
    { weight; name; arity; to_string }

  (** Compare two operations for equality. *)
  let eq f1 f2 =
    f1.name = f2.name && f1.arity = f2.arity && f1.weight = f2.weight

  let to_string f a = f.to_string a
end

(** Variables. *)
module Var = struct
  (** A variable. *)
  (* Note: the integer is only here to implement compare, and should not be used
     otherwise. *)
  type t = int ref

  (** Create a fresh variable. *)
  let fresh : unit -> t =
    let n = ref (-1) in
    fun () ->
      incr n; ref !n

  (** Equality between variables. *)
  let eq (x:t) (y:t) = x == y

  let compare (x:t) (y:t) = compare x y

  (** Create a function which will assign names to variables. *)
  let namer () =
    let f = Utils.namer eq in
    fun x -> "x" ^ String.subscript (string_of_int (f x))

  let namer_natural () =
    let f = Utils.namer eq in
    let name = [|"x"; "y"; "z"; "t"; "u"; "v"; "w"|] in
    fun x -> name.(f x)

  (** String representation of a variable. *)
  let to_string = namer ()
end

(** A variable. *)
type var = Var.t

(** A term. *)
type t =
    | App of Op.t * t list (** application *)
    | Var of var (** variable *)

type term = t

(** Create a fresh variable term. *)
let var () = Var (Var.fresh ())

(** Create an application. *)
let app f a =
  if (List.length a <> Op.arity f) then failwith (Printf.sprintf "%s expects %d arguments but %d provided" (Op.name f) (Op.arity f) (List.length a));
  App (f,a)

(** Simple parser for terms and applications. *)
let parser s =
  let rec term s =
    let s = String.trim s in
    try
      let n = String.index s '(' in
      let f = String.sub s 0 n in
      assert (s.[String.length s - 1] = ')');
      let a = String.sub s (n+1) (String.length s - (n+1) - 1) in
      let a =
        if a = "" then [] else
          let k = ref 0 in
          let p = function
            | '(' -> incr k; false
            | ')' -> decr k; false
            | ',' -> !k = 0
            | _ -> false
          in
          String.split_on_predicate p a
      in
      let a = List.map term a in
      `App (f, a)
    with
    | Not_found -> `Var s
  in
  let step s =
    let s = String.trim s in
    if s.[String.length s - 1] = '-' then `Inv (term (String.sub s 0 (String.length s - 1)))
    else term s
  in
  let l = String.split_on_char '.' s in
  if List.length l = 1 then step (List.hd l)
  else `Seq (List.map step l)

let parse_var =
  let vars = ref [] in
  fun x ->
    if not (List.mem_assoc x !vars) then vars := (x, Var.fresh ()) :: !vars;
    List.assoc x !vars

(** Parse a term. *)
let parse ops s =
  let rec aux = function
    | `App (f, a) ->
      let f = List.find (fun o -> Op.name o = f) ops in
      let a = List.map aux a in
      app f a
    | `Var x ->
      Var (parse_var x)
    | `Seq _ -> assert false
    | `Inv _ -> assert false
  in
  aux (parser s)

(** Equality between terms. *)
let rec eq t1 t2 =
  match t1, t2 with
  | Var x, Var y -> Var.eq x y
  | App (f1, a1), App (f2, a2) ->
    Op.eq f1 f2 &&
    List.length a1 = List.length a2 &&
    List.for_all2 eq a1 a2
  | _ -> false

let eq_term = eq

(** String representation of a term. *)
let rec to_string ?(var=Var.to_string) = function
  | App (f, a) -> Op.to_string f (List.map (to_string ~var) a)
  | Var x -> var x

let string_of_term ?var = to_string ?var

(** Is a term a variable? *)
let is_var = function
  | Var _ -> true
  | _ -> false

let get_var = function
  | Var x -> x
  | _ -> raise Not_found

(** Variables in a term. *)
let vars t =
  let rec aux vars = function
    | App (_, a) ->
      List.fold_left (fun vars t -> aux vars t) vars a
    | Var x ->
      if List.exists (Var.eq x) vars then vars
      else x::vars
  in
  List.rev (aux [] t)

(** Whether a variable occurs in a term. *)
let rec occurs x = function
  | App (_, a) -> List.exists (occurs x) a
  | Var y -> Var.eq x y

(** Generate all terms with given number of operations. *)
let generate_ops ?(vars=[]) ops n =
  (* Perform the generation of all cases, given a list of already used vars, returning the terms along with known variables. *)
  let rec aux vars n =
    if n = 0 then
      let x = Var.fresh () in
      (Var x, x::vars)::(List.map (fun x -> Var x, vars) (x::vars))
    else
      List.map
        (fun f ->
           List.map (fun (l, vars) -> App (f,l), vars) (args vars (n-1) (Op.arity f))
        ) ops |> List.flatten
  (* Genrate k arguments, totaling n operations, with given vars. Also returns the used variables. *)
  and args vars n k =
    if n < 0 then []
    else if k = 0 then
      if n > 0 then []
      else [[], vars]
    else
      List.init (n+1)
        (fun i ->
           List.map
             (fun (t, vars) ->
                List.map (fun (l, vars) -> t::l, vars) (args vars (n-i) (k-1))
             ) (aux vars i) |> List.flatten
        ) |> List.flatten
  in
  aux vars n |> List.map fst

(** Terms as an alphabet. *)
module TermAlphabet : Alphabet.T with type t = term = struct
  type t = term
  
  let eq = eq

  let compare _ _ = failwith "TODO"

  let to_string t = to_string t
end

(** Lexicographic path order on terms. *)
module LPO = struct
  let rec gt ge_op t u =
    (* Printf.printf "%s > %s ?\n%!" (to_string t) (to_string u); *)
    match t, u with
    | t, Var x -> not (eq t u) && occurs x t
    | App (f,a), App (g, b) ->
      if List.exists (fun t -> ge ge_op t u) a then true else
      if Op.eq f g then
        (* lexicographic > *)
        let rec lex l1 l2 =
          match l1, l2 with
          | x1::l1, x2::l2 when eq x1 x2 -> lex l1 l2
          | x1::l1, x2::l2 when gt ge_op x1 x2 -> true
          | _, _ -> false
        in
        List.for_all (fun u -> gt ge_op t u) b &&
        lex a b
      else if ge_op f g then List.for_all (fun u -> gt ge_op t u) b
      else false
    | _ -> false

  and ge ge_op t u =
    eq t u || gt ge_op t u
end

(** Substitutions. *)
module Substitution = struct
  (** A substitution. *)
  type t = (var * term) list

  let to_string ?var s =
    "[" ^ String.concat "," (List.map (fun (x,t) -> to_string ?var t ^ "/" ^ to_string ?var (Var x)) s) ^ "]"

  (** Emtpy substitution. *)
  let empty : t = []

  (** Identity substition. *)
  let id vars : t =
    List.map (fun x -> x, Var x) vars

  (** Renaming of variables. *)
  let rename vars : t =
    List.map (fun x -> x, Var (Var.fresh ())) vars

  let simple x t : t = [x,t]

  let add (s:t) x t : t = (x,t)::s

  (** Find the value associated to a variable. *)
  let rec find (s:t) x =
    match s with
    | (y,t)::s -> if Var.eq x y then t else find s x
    | [] -> raise Not_found

  (** Apply a substitution to a term. *)
  let rec app (s:t) = function
    | App (g, a) -> App (g, List.map (app s) a)
    | Var x ->
      try find s x
      with Not_found -> Var x

  (** Compose substitutions. *)
  let compose (s:t) (s':t) : t =
    List.map (fun (x,t) -> x, app s' t) s

  (** Equality of substitutions. *)
  let eq s1 s2 =
    let included (s1:t) (s2:t) =
      List.for_all (fun (x,t) -> eq t (app s2 (Var x))) s1
    in
    included s1 s2 && included s2 s1

  (** Is a substitution a renaming of variables? *)
  let is_renaming (s:t) =
    List.for_all (fun (x,t) -> is_var t) s

  let is_injective_renaming (s:t) =
    let rec aux vars = function
      | [] -> true
      | (_,t)::s ->
        if is_var t then
          let x = get_var t in
          if List.exists (Var.eq x) vars then false
          else aux (x::vars) s
        else false
    in
    aux [] s

  (** Inverse of a renaming. *)
  let inv (s:t) : t =
    List.map
      (fun (x,t) ->
        match t with
        | Var y -> y, Var x
        | _ -> failwith "Not inversible."
      ) s

  (** Domain of a substitution. *)
  let domain (s:t) =
    List.map fst s

  let in_dom (s:t) x =
    List.exists (fun (y,_) -> Var.eq y x) s

  (** Restrict the domain of a substitution. *)
  let restrict vars (s:t) : t =
    List.filter (fun (x,_) -> List.exists (Var.eq x) vars) s
end

module Subst = Substitution

type subst = Subst.t

(** Renamings of variables. *)
module Renaming = struct
  (** A renaming. *)
  type t = (var * var) list

  (** Empty renaming. *)
  let empty : t = []

  let add (s:t) x y = (x,y)::s

  (** Convert to subtitution. *)
  let to_substitution (s : t) : Substitution.t =
    List.map (fun (x,y) -> x, Var y) s

  (** Find the renaming of a variable. *)
  let rec find_opt (s:t) x =
    match s with
    | (x',y)::s -> if Var.eq x x' then Some y else find_opt s x
    | [] -> None

  (** Raised when unification fails. *)
  exception Unification

  (** Unify two terms given a partial renaming, returning the extended renaming. *)
  let rec unify (s:t) t u =
    match t, u with
    | Var x, Var y ->
      (
        match find_opt s x with
        | Some y' -> if Var.eq y y' then s else raise Unification
        | None -> add s x y
      )
    | App (f,l), App (f',l') ->
      if not (Op.eq f f' && List.length l = List.length l') then raise Unification else
        List.fold_left2 unify s l l'
    | Var _, App _
    | App _, Var _ -> raise Unification

  (** Same a [unify] but returning an option instead of raising an error. *)
  let unify_opt s t u =
    try Some (unify s t u)
    with Unification -> None
end

(** Interpretation of terms into cartesian categories. *)
module Interpretation = struct
  (** Polynomial interpretations. *)
  module Polynomial = struct
    (* Words. *)
    module W = Monoid.Multisets(Var)
    (* Polynomials. *)
    module P = struct
      module R = Ring.Int
      include Algebra.OverRing.Free(R)(W)

      let is_commutative = W.is_commutative

      let to_string p =
        if eq p zero then "0" else
          let ans = ref "" in
          let w s = ans := !ans ^ s in
          iter (fun a u ->
              let a =
                if a < 0 then (w "-"; -a)
                else if !ans <> "" then (w "+"; a) else a
              in
              if R.eq a R.one && W.eq u W.one then w "1"
              else
                (
                  if not (R.eq a R.one) then w (R.to_string a);
                  if not (W.eq u W.one) then w (W.to_string u)
                )
            ) p;
          !ans

      (** Canonical injection of variables into polynomials. *)
      let var x = inj (W.inj x)

      let pow (u:t) n = Monoid.simple_pow one mul u n
    end
    (* Substitutions. *)
    module S = struct
      include W.Map(P)

      (** Apply a substitution to a polynomial. *)
      let bind (s : map) (p : P.t) =
        P.map (fun u -> bind s u) p
    end

    include P

    (** Extend an the interpretation of operations to terms. *)
    let rec interpretation (op:Op.t -> Var.t array -> P.t) (t:term) : t =
      match t with
      | App (f, l) ->
        (* Canonical name for variables. *)
        let v = List.init (Op.arity f) (fun i -> Var.fresh ()) in
        let s = List.map2 (fun x t -> x, interpretation op t) v l in
        let s = S.of_list s in
        S.bind s (op f (Array.of_list v))
      | Var x -> P.var x
  end
end

exception Not_unifiable

(** Most general unifier. *)
let unify t1 t2 =
  (* Printf.printf "UNIFY %s WITH %s\n%!" (to_string t1) (to_string t2); *)
  let rec aux q s =
    match q with
    | [] -> s
    | p::q ->
       match p with
       | Var x, t ->
          if occurs x t then raise Not_unifiable;
          let s' = Subst.simple x t in
          let f = Subst.app s' in
          let q = List.map (fun (t1,t2) -> f t1, f t2) q in
          let s = Subst.compose s s' in
          aux q (Subst.add s x t)
       | t, Var x -> aux ((Var x,t)::q) s
       | App (f1,a1), App (f2,a2) ->
          if not (Op.eq f1 f2) then raise Not_unifiable;
          let q = (List.map2 pair a1 a2) @ q in
          aux q s
  in
  let s = aux [t1,t2] Subst.empty in
  assert (eq (Subst.app s t1) (Subst.app s t2));
  s

(*
let unify t1 t2 =
  let s = unify t1 t2 in
  let var = Var.namer () in
  let t1 = to_string ~var t1 in
  let t2 = to_string ~var t2 in
  let ss = Subst.to_string ~var s in
  Printf.printf "UNIFY %s WITH %s IS %s\n%!" t1 t2 ss;
  s
*)

(** Whether a pattern matches a term. *)
let matches t1 t2 =
  let rec aux q s =
    match q with
    | [] -> s
    | p::q ->
      match p with
      | Var x, t ->
        if Subst.in_dom s x then (if eq (Subst.app s (Var x)) t then aux q s else raise Not_unifiable)
        else aux q (Subst.add s x t)
      | _, Var _ -> raise Not_unifiable
      | App (f1,a1), App (f2,a2) ->
        if not (Op.eq f1 f2) then raise Not_unifiable;
        let q = (List.map2 pair a1 a2)@q in
        aux q s
  in
  aux [t1,t2] Subst.empty

(*
let matches t1 t2 =
  let s = matches t1 t2 in
  Printf.printf "MATCH %s WITH %s IS %s\n%!" (to_string t1) (to_string t2) (Subst.to_string s);
  s
 *)

(** Whether two terms are alpha-equivalent *)
let equivalent ?(s=Subst.empty) (t1:t) (t2:t) =
  let rec aux q s =
    match q with
    | [] -> s
    | p::q ->
      match p with
      | Var x, Var y ->
        if Subst.in_dom s x then (if eq (Subst.app s (Var x)) (Var y) then aux q s else raise Not_unifiable)
        else
          let s = Subst.add s x (Var y) in
          aux q s
      | _, Var _ | Var _, _ -> raise Not_unifiable
      | App (f1,a1), App (f2,a2) ->
        if not (Op.eq f1 f2) then raise Not_unifiable;
        let q = (List.map2 pair a1 a2)@q in
        aux q s
  in
  try ignore (aux [t1,t2] s); true
  with Not_unifiable -> false

(** Rewriting systems. *)
module RS = struct
  let list_remove_nth n l =
    let rec aux n p = function
      | x::l -> if n = 0 then List.rev p, l else aux (n-1) (x::p) l
      | [] -> assert false
    in
    aux n [] l

  (** Rewriting rules. *)
  module Rule = struct
    (** A rewriting rule. *)
    type t = string * term * term

    let make r s t : t = (r,s,t)

    let of_string ops r s t =
      let s = parse ops s in
      let t = parse ops t in
      make r s t

    let name ((r,s,t):t) = r

    let source ((r,s,t):t) = s

    let target ((r,s,t):t) = t

    (** Variables of the rule. *)
    let vars r = vars (source r)

    (** Arity of the rule. *)
    let arity r = List.length (vars r)

    (** Arguments of a rule, sorted according to its variables. *)
    let args r s =
      let vars = vars r in
      let args = List.sort (fun (x,t) (y,u) -> List.index (fun z -> Var.eq z x) vars - List.index (fun z -> Var.eq z y) vars) s in
      List.map snd args

    (** Substitution from arguments. *)
    let args_subst r a : Subst.t =
      List.map2 (fun x t -> x,t) (vars r) a

    (** String representation of a rule. *)
    let to_string ?var r =
      let s = to_string ?var (source r) in
      let t = to_string ?var (target r) in
      name r ^ " : " ^ s ^ " -> " ^ t

    (** Equality of rules. *)
    (*
    let eq (r1:t) (r2:t) =
      (* Printf.printf "EQ %s WITH %s\n%!" (to_string r1) (to_string r2); *)
      if name r1 <> name r2 then false else
        try
          let s = equivalent (source r1) (source r2) in
          eq (Subst.app s (target r1)) (target r2)
        with
        | Not_unifiable -> false
    *)
    let eq r1 r2 = name r1 = name r2
  end

  type rule = Rule.t

  (** A rewriting system. *)
  type t =
    {
      operations : Op.t list;
      rules : rule list
    }
  type rs = t

  let operations rs = rs.operations

  let rules rs = rs.rules

  (** Select rules satisfying a predicate. *)
  let filter p rs = { operations = rs.operations; rules = List.filter p rs.rules }

  let make operations rules =
    { operations; rules }

  (** Empty rewriting system. *)
  let empty ops = make ops []

  let to_string ?(var=Var.namer) rs =
    String.concat "\n" (List.map (fun r -> Rule.to_string ~var:(var()) r) (rules rs))

  (** Find the rule with given name. *)
  let find rs r =
    List.find (fun r' -> Rule.name r' = r) (rules rs)

  (** Rewriting steps. *)
  module Step = struct
    (** A rewriting step. *)
    type t =
      | TApp of Op.t * term list * t * term list (** Term application. *)
      | RApp of Rule.t * Subst.t (** Rule application. *)

    let of_term _ = assert false

    let tapp f a1 s a2 =
      assert (List.length a1 + 1 + List.length a2 = Op.arity f);
      TApp (f, a1, s, a2)

    let rapp r s =
      (* Printf.printf "rapp: %s to %s\n%!" (Rule.to_string r) (Subst.to_string s); *)
      let vr = Rule.vars r in
      let vs = Subst.domain s in
      assert (List.for_all (fun x -> List.exists (Var.eq x) vs) vr);
      assert (List.for_all (fun x -> List.exists (Var.eq x) vr) vs);
      RApp (r, s)

    (** Apply a substitution. *)
    let rec subst s = function
      | TApp (f, a1, st, a2) -> tapp f (List.map (Subst.app s) a1) (subst s st) (List.map (Subst.app s) a2)
      | RApp (r, s') -> rapp r (Subst.compose s' s)

    (** Source. *)
    let rec source = function
      | TApp (f, a1, s, a2) -> app f (a1@[source s]@a2)
      | RApp (r, s) -> Subst.app s (Rule.source r)

    (** Target. *)
    let rec target = function
      | TApp (f, a1, s, a2) -> app f (a1@[target s]@a2)
      | RApp (r, s) -> Subst.app s (Rule.target r)

    let rec label ?(var=Var.to_string) = function
      | TApp (f, a1, s, a2) ->
        let a1 = List.map (string_of_term ~var) a1 in
        let a2 = List.map (string_of_term ~var) a2 in
        let a = a1@[label ~var s]@a2 in
        Op.to_string f a
      | RApp (r, s) ->
        let a = Rule.args r s in
        let a = List.map (string_of_term ~var) a in
        let a = String.concat "," a in
        Rule.name r ^ "(" ^ a ^ ")"

    let to_string ?var s =
      string_of_term ?var (source s) ^ " -" ^ label ?var s ^ "-> " ^ string_of_term ?var (target s)

    let rec rule = function
      | TApp (f, a1, s, a2) -> rule s
      | RApp (r, _) -> r

    let has_context = function
      | TApp _ -> true
      | RApp (r, s) -> not (Subst.is_injective_renaming s)

    let rec eq s1 s2 =
      (* Printf.printf "eq: %s vs %s\n%!" (to_string s1) (to_string s2); *)
      match s1,s2 with
      | TApp (f, a1, s, a2), TApp (f', a1', s', a2') ->
        Op.eq f f' &&
        List.length a1 = List.length a1' &&
        List.length a2 = List.length a2' &&
        List.for_all2 eq_term a1 a1' &&
        eq s s' &&
        List.for_all2 eq_term a2 a2'
      | RApp (r, s), RApp (r', s') -> Rule.eq r r' && Subst.eq s s'
      | _ -> false
    
    (** Whether a rule occurs in a step. *)
    let rec has_rule r = function
      | TApp (_, _, s, _) -> has_rule r s
      | RApp (r', _) -> Rule.eq r r'
  end

  type step = Step.t

  module StepSpan (* : ARS.Span *) = struct
    include Step
    type obj = term
    let to_string s = to_string s
  end

  (** Underlying abstract rewriting system. *)
  module ARS = ARS.Make(TermAlphabet)(StepSpan)

  (** All possible rewriting steps. *)
  let steps (rs:t) t : step list =
    let rec aux r ctx = function
      | Var x -> []
      | App (f,a) as t ->
         let s =
           try
             let s = matches (Rule.source r) t in
             [ctx (Step.rapp r s)]
           with
           | Not_unifiable -> []
         in
         let s' =
           List.mapi
             (fun i t ->
                let a1, a2 = list_remove_nth i a in
                let ctx t = ctx (Step.tapp f a1 t a2) in
                aux r ctx t
             ) a
         in
         let s' = List.flatten s' in
         s' @ s
    in
    List.flatten (List.map (fun r -> aux r Fun.id t) (rules rs))

  (** Rewriting paths. *)
  module Path = struct
    include ARS.Path
 
    let rec to_string ?var = function
      | Empty t -> string_of_term ?var t
      | Step (p,s) ->
        let src = to_string ?var p in
        let lbl = Step.label ?var s in
        let tgt = string_of_term ?var (Step.target s) in
        src ^ " -" ^ lbl ^ "-> " ^ tgt

    (** Rules which are used without context. *)
    let rec toplevel_rules = function
      | Empty t -> []
      | Step (p,s) -> (toplevel_rules p)@(if Step.has_context s then [] else [Step.rule s])

    (** Rules occurring in a step. *)
    let rec rules = function
      | Step (p,s) ->
        let r = Step.rule s in
        let rr = rules p in
        if List.exists (Rule.eq r) rr then rr
        else r::rr
      | Empty _ -> []

    (** nth step in a path. *)
    let nth_step n p =
      let rec aux n = function
        | Step (p, s) ->
          if n = 0 then s else aux (n-1) p
        | Empty _ -> assert false
      in
      aux (length p - 1 - n) p

    (** nth term in a path. *)
    let nth_term n p =
      let rec aux n = function
        | Step (p, s) ->
          if n = 0 then Step.target s else aux (n-1) p
        | Empty t ->
          if n = 0 then t else assert false
      in
      aux (length p - n) p
  end

  (** Normalize a term. *)
  let normalize rs t =
    let rec aux p =
      let s = steps rs (Path.target p) in
      if s = [] then p else aux (Path.append_step p (List.hd s))
    in
    aux (Path.empty t)

  (** Unify the source of [r2] with a subterm of the source of [r1]. *)
  let critical_rules r1 r2 =
    let rec aux ctx t =
      match t with
      | Var x -> []
      | App (f,a) ->
        let s =
          try
            let t2 = Rule.source r2 in
            (* We rename rule 2 before unification. Note that we cannot rename
               rule 1 because some of its variables can occur in the context. *)
            let n2 = Subst.rename (Rule.vars r2) in
            let s = unify t (Subst.app n2 t2) in
            (* Extend the domain of definition. *)
            let step1 = Step.rapp r1 (Subst.compose (Subst.id (Rule.vars r1)) s) in
            let step2 = Step.subst s (ctx (Step.rapp r2 n2)) in
            assert (equivalent (Step.source step1) (Step.source step2));
            if Rule.eq r1 r2 && not (Step.has_context step1) && not (Step.has_context step2) then [] else [step1,step2]
          with
          | Not_unifiable -> []
        in
        let s' =
          List.mapi
            (fun i t ->
               let a1, a2 = list_remove_nth i a in
               let ctx t = ctx (Step.tapp f a1 t a2) in
               aux ctx t) a
        in
        let s' = List.flatten s' in
        s @ s'
    in
    aux Fun.id (Rule.source r1)

  (** Critical branchings. *)
  let critical (rs:t) =
    let steps = List.flatten (List.flatten (List.map (fun r1 -> List.map (fun r2 -> critical_rules r1 r2) (rules rs)) (rules rs))) in
    (* Remove symmetric pairs. *)
    let rec sym = function
      | (s1,s2)::steps ->
         let steps = List.filter (fun (s2',s1') -> not (Step.eq s1 s1' && Step.eq s2 s2')) steps in
         (s1,s2)::(sym steps)
      | [] -> []
    in
    sym steps

  (** Orient rules according to a particular order. *)
  let orient ~gt rs =
    let rules = List.map (fun (n,t,u) -> if not (gt t u) then (n,u,t) else (n,t,u)) (rules rs) in
    { operations = rs.operations; rules }

  (** Knuth-Bendix completion. [gt] is the strict order on terms, [callback] is a function which is called regularly with the current rewriting system as argument (useful to display during the completion). *)
  let knuth_bendix ?(gt=LPO.gt (>=)) ?namer ?(callback=fun _ -> ()) rs =
    let rs = orient ~gt rs in
    (* Name for new rules. *)
    let namer =
      match namer with
      | Some namer -> namer
      | None ->
        let n = ref (-1) in
        fun () -> incr n; "K"^string_of_int !n
    in
    (* Rules to handle. *)
    let queue = ref rs.rules in
    (* Produced rewriting system. *)
    let rules = ref rs.rules in
    let add (r:Rule.t) =
      (* Printf.printf "add %s\n%!" (Rule.to_string r); *)
      rules := r :: !rules;
      (* Normalize the rules *)
      rules :=
        List.map
          (fun ((n,s,t) as r) ->
             (* TODO: proper recursive function instead of this filter *)
             let rules = List.filter (fun r' -> not (Rule.eq r r')) !rules in
             let rs = { rs with rules } in
             n, Path.target (normalize rs s), Path.target (normalize rs t)
          ) !rules;
      rules := List.filter (fun (n,s,t) -> not (eq s t)) !rules;
      queue := !queue@[r]
      (* queue := r :: !queue *)
    in
    while !queue <> [] do
      let r = List.hd !queue in
      queue := List.tl !queue;
      let cp = List.flatten (List.map (fun s -> (critical_rules r s)@(critical_rules s r)) !rules) in
      List.iter
        (fun (s1, s2) ->
           let rs = { rs with rules = !rules } in
           let p1 = Path.append (Path.step s1) (normalize rs (Step.target s1)) in
           let p2 = Path.append (Path.step s2) (normalize rs (Step.target s2)) in
           let t1 = Path.target p1 in
           let t2 = Path.target p2 in
           if not (eq t1 t2) then
             let t1, t2 = if gt t1 t2 then t1, t2 else t2, t1 in
             let r = (namer (), t1, t2) in
             Printf.printf "add %s\n%s\n%s\n\n%!" (Rule.to_string r) (Path.to_string p1) (Path.to_string p2);
             add r
        ) cp;
      callback { rs with rules = !rules }
    done;
    { rs with rules = !rules }

  (** Raised when the system is not confluent. *)
  exception Not_confluent

  (** Squier completion. *)
  let squier rs =
    List.map
      (fun (s1,s2) ->
         (* Printf.printf "branching\n%s\n%s\n\n%!" (Step.to_string s1) (Step.to_string s2); *)
         let p1 = Path.append (Path.step s1) (normalize rs (Step.target s1)) in
         let p2 = Path.append (Path.step s2) (normalize rs (Step.target s2)) in
         if not (eq (Path.target p1) (Path.target p2)) then
           (
             Printf.printf "not confluent:\n%s\n%s\n%!" (Path.to_string p1) (Path.to_string p2);
             raise Not_confluent
           );
         p1, p2
      )
      (critical rs)

  (** Rewriting zigzags. *)
  module Zigzag = struct
    include ARS.Zigzag

    (** String representation. *)
    let rec to_string ?(pa=false) ?var = function
      | Step s -> Step.label ?var s
      | Comp (p1,p2) ->
        let s = to_string ~pa:true ?var p1 ^ "." ^ to_string ?var p2 in
        if pa then "(" ^ s ^ ")" else s
      | Id t -> string_of_term ?var t
      | Inv p -> to_string ~pa:true ?var p ^ "-"

    (** Apply a substitution. *)
    let rec subst s = function
      | Step t -> Step (Step.subst s t)
      | Comp (p1, p2) -> Comp (subst s p1, subst s p2)
      | Id t -> Id (Subst.app s t)
      | Inv p -> Inv (subst s p)

    (** Number of occurences of a given rule in a path. *)
    let rec rule_occurences r = function
      | Step s -> if Step.has_rule r s then 1 else 0
      | Comp (p, q) -> rule_occurences r p + rule_occurences r q
      | Id _ -> 0
      | Inv p -> rule_occurences r p

    (** Number of occurences of a given rule in a path, counted negatively when inverted. *)
    let rec rule_algebraic_occurences r = function
      | Step s -> if Step.has_rule r s then 1 else 0
      | Comp (p, q) -> rule_algebraic_occurences r p + rule_algebraic_occurences r q
      | Id _ -> 0
      | Inv p -> - rule_algebraic_occurences r p

    (** Whether a path contains a rule. *)
    let has_rule r p = rule_occurences r p > 0

    (** Express a rule as a zigzag in a cell. *)
    let value r p =
      let p = canonize p in
      (* Printf.printf "value of %s in %s\n%!" (Rule.name r) (to_string p); *)
      assert (rule_occurences r p = 1);
      let p = if rule_algebraic_occurences r p = - 1 then canonize (inv p) else p in
      let rec aux prefix = function
        | Comp (Step (RApp (r', s)), p) when Rule.eq r r'->
          assert (not (has_rule r p));
          assert (Subst.is_renaming s);
          let prefix = concat (List.rev prefix) in
          subst (Subst.inv s) (concat [inv prefix; inv p])
        | Comp (Step _ as s, p) | Comp (Inv (Step _) as s, p) -> aux (s::prefix) p
        | Step _ | Inv (Step _) as s -> aux prefix (Comp (s, Id (target s)))
        | Id _ -> assert false
        | _ -> assert false
      in
      let v = canonize (aux [Id (source p)] p) in
      assert (eq_term (source v) (Rule.source r));
      assert (eq_term (target v) (Rule.target r));
      v

    (** Replace a rule by a path in a path. *)
    let rec replace_rule r (pr:t) (p:t) =
      (* Printf.printf "replace_rule: %s\n%!" (to_string p); *)
      let rec replace_step tm_ctx rs_ctx = function
        | Step.TApp (f, a1, s, a2) ->
          let tm_ctx t = tm_ctx (app f (a1@[t]@a2)) in
          let rs_ctx s = rs_ctx (Step.tapp f a1 s a2) in
          replace_step tm_ctx rs_ctx s
        | RApp (r', s) when Rule.eq r r' -> map tm_ctx rs_ctx (subst s pr)
        | RApp (r, s) -> step (rs_ctx (Step.rapp r s))
      in
      let replace_step = replace_step Fun.id Fun.id in
      match p with
      | Step s -> replace_step s
      | Comp (p, q) -> comp (replace_rule r pr p) (replace_rule r pr q)
      | Id t -> Id t
      | Inv p -> Inv (replace_rule r pr p)

    (** nth step in a path supposed to be in canonical form. *)
    let rec nth_step n = function
      | Step s when n = 0 -> true, s
      | Inv (Step s) when n = 0 -> false, s
      | Comp (Step s, p) -> if n = 0 then true, s else nth_step (n-1) p
      | Comp (Inv (Step s), p) -> if n = 0 then false, s else nth_step (n-1) p
      | _ -> assert false

    (** nth term in a path supposed to be in canonical form. *)
    let rec nth_term n = function
      | Step _ | Inv (Step _) as p ->
        if n = 0 then source p
        else if n = 1 then target p
        else assert false
      | Comp (Step _ as p, q) | Comp (Inv (Step _) as p, q) ->
        if n = 0 then source p else nth_term (n-1) q
      | _ -> assert false

    let parse rs s =
      let unid = function
        | Id t -> t
        | _ -> assert false
      in
      let rec aux = function
        | `App (f, a) ->
          (
            let a = List.map aux a in
            try
              (* Printf.printf "op: %s\n%!" f; *)
              let f = List.find (fun o -> Op.name o = f) (operations rs) in
              (* List.iter (fun p -> Printf.printf "%s\n%!" (to_string p)) a; *)
              if List.for_all is_id a then
                let a = List.map unid a in
                let t = app f a in
                Id t
              else
                let n = try List.index (fun p -> not (is_id p)) a with Not_found -> assert false in
                let t = List.nth a n in
                let a1, a2 = list_remove_nth n a in
                let a1 = List.map unid a1 in
                let a2 = List.map unid a2 in
                let tm_ctx t = app f (a1@[t]@a2) in
                let rs_ctx t = Step.tapp f a1 t a2 in
                map tm_ctx rs_ctx t
            with
            | Not_found ->
              let r = find rs f in
              let a = List.map unid a in
              let s = List.map2 pair (Rule.vars r) a in
              Step (Step.rapp r s)
          )
        | `Inv p -> inv (aux p)
        | `Seq l -> concat (List.map aux l)
        | `Var x ->
          let x = parse_var x in
          Id (Var x)
      in
      aux (parser s)
  end

  (** Loops. *)
  module Loop = struct
    (** A (pointed) loop. *)
    type t = Zigzag.t

    let make (p:Zigzag.t) : t =
      assert (eq_term (Zigzag.source p) (Zigzag.target p));
      p

    let to_string = Zigzag.to_string

    let comp = Zigzag.comp

    let inv = Zigzag.inv

    let of_cell p1 p2 =
      assert (eq_term (Zigzag.source p1) (Zigzag.source p2));
      assert (eq_term (Zigzag.target p1) (Zigzag.target p2));
      comp p1 (inv p2)

    let canonize = Zigzag.canonize

    let value = Zigzag.value

    let replace_rule = Zigzag.replace_rule

    let length = Zigzag.length

    let rotate k p =
      let n = length p in
      let k = Int.modulo k n in
      let l = Zigzag.to_list p in
      let l1 = List.sub l 0 k in
      let l2 = List.sub l k (n-k) in
      let l1 = Zigzag.concat l1 in
      let l2 = Zigzag.concat l2 in
      Zigzag.comp l2 l1
  end

  (** Coherence cells. *)
  module Coherence = struct
    type t =
      {
        name : string;
        loop : Loop.t;
      }

    let name c = c.name

    let loop c = c.loop

    let make name loop =
      { name; loop }

    let to_string ~var c =
      let p = Loop.canonize (loop c) in
      Printf.sprintf "%s: %s\n" (name c) (Loop.to_string ~var p)

    let rotate n c =
      { c with loop = Loop.rotate n (loop c) }

    let value r c =
      Loop.value r (loop c)

    let replace_rule r v c =
      { c with loop = Loop.replace_rule r v (loop c) }

    module Path = struct
    end
  end

  (** Coherent presentations. *)
  module Coherent = struct
    (** A coherent presentation. *)
    type t =
      {
        rs : rs;
        coherence : Coherence.t list;
      }

    type crs = t

    (** Underlying rewriting system. *)
    let rs crs = crs.rs

    let rules crs = rules (rs crs)

    let coherence crs = crs.coherence

    let set_rules crs rules = { crs with rs = { crs.rs with rules } }

    let to_string ?(var=Var.namer) crs =
      let coherence =
        List.map
          (fun c ->
             Coherence.to_string ~var:(var ()) c
          ) crs.coherence
      in
      let coherence = String.concat "\n" coherence in
      coherence

    let to_tex ?(var=Var.namer_natural) crs =
      let ans = ref "" in
      let print s = Printf.ksprintf (fun s -> ans := !ans ^ s) s; in
      print "\\documentclass[a4paper,9pt]{extarticle}\n\
             \\usepackage[utf8x]{inputenc}\n\
             \\usepackage{amsmath}\n\
             \\usepackage{tikz-cd}\n\
             \\usepackage[margin=1cm,includefoot]{geometry}\n\
             \\title{Coherent presentation}\n\
             \\author{ocaml-alg}\n\n\
             \\begin{document}\n\
             \\maketitle\n\n";
      let rules =
        List.map
          (fun r ->
             let var = var () in
             let s = string_of_term ~var (Rule.source r) in
             let t = string_of_term ~var (Rule.target r) in
             Printf.sprintf "%s &: %s \\to %s\\\\" (Rule.name r) s t
          ) (rules crs)
      in
      let rules = String.concat "\n" rules in
      print "\\section{Rules}\n\
             \n\
             \\begin{align*}\n\
             %s\n\
             \\end{align*}\n\
             \n" rules;
      print "\\section{Coherence}\n\n";
      List.iter
        (fun c ->
           let p = Loop.canonize (Coherence.loop c) in
           Printf.printf "print %s: %s\n%!" (Coherence.name c) (Loop.to_string p);
           let p1, p2 =
             let l = Zigzag.to_list p in
             if l <> [] then ignore (Zigzag.concat l); (* test *)
             let n = List.length l in
             if n = 0 then p, p else
               try
                 (* Try to split forward / backward. *)
                 let k = List.index Zigzag.is_inv l in
                 if k = 0 then raise Exit;
                 let l1 = List.sub l 0 k in
                 let l2 = List.sub l k (n-k) in
                 if not (List.for_all Zigzag.is_inv l2) then raise Exit;
                 let p1 = Zigzag.concat l1 in
                 let p2 = Zigzag.canonize (Zigzag.inv (Zigzag.concat l2)) in
                 let p1, p2 =
                   if List.length l1 > List.length l2 then
                     p2, p1
                   else
                     p1, p2
                 in
                 Zigzag.canonize p1, Zigzag.canonize p2
               with
               | Exit | Not_found ->
                 (* By default, split in k/k or k/k+1. *)
                 let l1 = List.sub l 0 (n/2) in
                 let l2 = List.sub l (n/2) (n-n/2) in
                 Zigzag.concat l1, Zigzag.canonize (Zigzag.inv (Zigzag.concat l2))
           in
           Printf.printf "split: %s / %s\n%!" (Zigzag.to_string p1) (Zigzag.to_string p2);
           let var = var () in
           let st n p =
             let d, s = Zigzag.nth_step n p in
             let d = if d then "" else "<-," in
             let s = Step.label ~var s in
             d ^ "\"{" ^ s ^ "}\""
           in
           let tm n p = string_of_term ~var (Zigzag.nth_term n p) in
           let cd () =
             match Zigzag.length p1, Zigzag.length p2 with
             | 1, 1 ->
               print "%s\\ar[d,bend right,%s']\\ar[d,bend left,%s]\\\\\n%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p1)
             | 1, 2 ->
               print "%s\\ar[dr,%s']\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2)
             | 1, 3 ->
               print "%s\\ar[drr,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2)
             | 1, 4 ->
               print "%s\\ar[ddrr,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2)
             | 1, 5 ->
               print "%s\\ar[dddrr,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2)
             | 1, 6 ->
               print "%s\\ar[ddddrr,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2) (st 5 p2) (tm 6 p2)
             | 2, 2 ->
               print "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[d,%s]\\\\\n%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 1 p1) (st 1 p1) (tm 2 p1)
             | 2, 3 ->
               print "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n%s\\ar[rr,%s']&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 1 p1) (st 1 p1) (tm 2 p1)
             | 2, 4 ->
               print "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n%s\\ar[rrr,%s']&&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 1 p1) (st 1 p1) (tm 2 p1)
             (*
             | 3, 3 ->
               print "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 4 ->
               print "%s\\ar[dd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             *)
             | 3, 5 ->
               print "%s\\ar[ddd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 8 ->
               print "%s\\ar[dddddd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2) (st 5 p2) (tm 6 p2) (st 6 p2) (tm 7 p2) (st 7 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 9 ->
               print "%s\\ar[ddddddd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2) (st 5 p2) (tm 6 p2) (st 6 p2) (tm 7 p2) (st 7 p2) (tm 8 p2) (st 8 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 10 ->
               print "%s\\ar[dddddddd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2) (st 5 p2) (tm 6 p2) (st 6 p2) (tm 7 p2) (st 7 p2) (tm 8 p2) (st 8 p2) (tm 9 p2) (st 9 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             (*
             | 5, 6 ->
               print "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[d,%s']&&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[d,%s']&&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[d,%s']&&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[rr,%s']&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2)
                 (tm 1 p1) (st 1 p1) (tm 3 p2) (st 3 p2)
                 (tm 2 p1) (st 2 p1) (tm 4 p2) (st 4 p2)
                 (tm 3 p1) (st 3 p1) (tm 5 p2) (st 5 p2)
                 (tm 4 p1) (st 4 p1) (tm 6 p2)
             *)
             | l1, l2 when l1 >= 2 && l2 = l1 + 1 ->
               print "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n" (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2);
               for i = 1 to l1 - 2 do
                 print "%s\\ar[d,%s']&&%s\\ar[d,%s]\\\\\n" (tm i p1) (st i p1) (tm (i+2) p2) (st (i+2) p2)
               done;
               let i = l1 - 1 in
               print "%s\\ar[rr,%s']&&%s" (tm i p1) (st i p1) (tm (i+2) p2)
             | l1, l2 when l1 >= 3 && l2 = l1 ->
               print "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n" (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2);
               for i = 1 to l1 - 3 do
                 print "%s\\ar[d,%s']&&%s\\ar[d,%s]\\\\\n" (tm i p1) (st i p1) (tm (i+2) p2) (st (i+2) p2)
               done;
               let i = l1 - 2 in
               print "%s\\ar[r,%s']&%s\\ar[r,%s']&%s" (tm i p1) (st i p1) (tm (i+1) p1) (st (i+1) p1) (tm (i+2) p2)
             | l1, l2 ->
               let p = Zigzag.canonize (Zigzag.append p1 (Zigzag.inv p2)) in
               Printf.printf "TODO: %d, %d\n" l1 l2;
               if Zigzag.is_id p then print "%s" (Zigzag.to_string ~var p) else
                 let l = Zigzag.length p in
                 Printf.printf "zzlen: %d\n%!" l;
                 let n = 2 in
                 for i = 0 to (l-1)/n do
                   print "%s" (tm (i*n) p);
                   for j = 0 to min n (l - i*n) - 1 do
                     print "\\ar[r,%s]&%s" (st (i*n+j) p) (tm (i*n+j+1) p)
                   done;
                   print "\\\\"
                 done
           in
           print "\\noindent\n\\subsection*{%s}\n" (Coherence.name c);
           (* print "\\vspace{-8ex}\n"; *)
           print "\\[\n\\begin{tikzcd}\n";
           cd ();
           print "\n\\end{tikzcd}\n\\]\n\n"
        ) (coherence crs);
      print "\\end{document}\n";
      !ans

    let view_pdf ?var rs =
      let fname, oc = Filename.open_temp_file "ocaml-alg" ".tex" in
      output_string oc (to_tex ?var rs);
      close_out oc;
      let cmd = Printf.sprintf "cd %s && pdflatex %s && evince `basename %s .tex`.pdf" (Filename.get_temp_dir_name ()) fname fname in
      assert (Sys.command cmd = 0)

    let make rs coherence =
      { rs; coherence }

    let find_rule crs r =
      find (rs crs) r

    (** Find coherence with given name. *)
    let find rs crs =
      List.find (fun c -> Coherence.name c = crs) rs.coherence

    let add_coherence crs c p =
      (* Printf.printf "add_coherence: %s / %s\n%!" (Zigzag.to_string p1) (Zigzag.to_string p2); *)
      assert (eq_term (Zigzag.source p) (Zigzag.target p));
      let coherence = (coherence crs)@[Coherence.make c p] in
      { crs with coherence }

    (** Rotate a coherence. *)
    let rotate crs cname n =
      let coherence = List.map (fun c -> if Coherence.name c = cname then Coherence.rotate n c else c) crs.coherence in
      { crs with coherence }

    (** Eliminate a rule with a coherence. *)
    let elim_rule crs r c =
      let r = find_rule crs r in
      (* let cname = c in *)
      let c = find crs c in
      let v = Coherence.value r c in
      let var = Var.namer_natural () in Printf.printf "\nelim rule: [%s] => %s\n%!" (Rule.to_string ~var r) (Zigzag.to_string ~var v);
      let rules = List.filter (fun r' -> not (Rule.eq r r')) (rules crs) in
      let coherence = List.map (fun c -> Coherence.replace_rule r v c) (coherence crs) in
      (* let coherence = List.filter (fun (c,_) -> c <> cname) coherence in *)
      { rs = { crs.rs with rules }; coherence }

    (** Morphisms between coherent presentations. *)
    module Morphism = struct
      type t =
        {
          source : crs;
          target : crs;
          mutable rules : (Rule.t * Zigzag.t) list;
          (* mulable coherences : (Coherence.t * ) *)
        }
    end
  end
end
