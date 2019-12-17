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

  let name f = f.name

  let weight f = f.weight

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
  type t = unit ref

  (** Create a fresh variable. *)
  let fresh () : t = ref ()

  (** Equality between variables. *)
  let eq (x:t) (y:t) = x == y

  (** Create a function which will assign names to variables. *)
  let namer () =
    let f = Utils.namer eq in
    fun x -> "x" ^ string_of_int (f x)

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
  assert (List.length a = Op.arity f);
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
  if List.length l = 1 then term (List.hd l)
  else `Seq (List.map step l)

(** Parse a term. *)
let parse ops vars s =
  let rec aux = function
    | `App (f, a) ->
      let f = List.find (fun o -> Op.name o = f) ops in
      let a = List.map aux a in
      app f a
    | `Var x ->
      if not (List.mem_assoc x !vars) then vars := (x, Var.fresh ()) :: !vars;
      let x = List.assoc x !vars in
      Var x
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

  let add s x t : t = (x,t)::s

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
      List.for_all (fun (x,t) -> eq t (app s1 (Var x))) s1
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
  let rec list_remove_nth n l =
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

    let of_string ops vars r s t =
      let s = parse ops vars s in
      let t = parse ops vars t in
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
  type t = rule list

  (** Empty rewriting system. *)
  let empty : t = []

  let to_string ?(var=Var.namer) rs =
    String.concat "\n" (List.map (fun r -> Rule.to_string ~var:(var()) r) rs)

  (** Find the rule with given name. *)
  let find_rule rs r =
    List.find (fun r' -> Rule.name r' = r) rs

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
    List.flatten (List.map (fun r -> aux r id t) rs)

  (** Rewriting paths. *)
  module Path = struct    
    (** A rewriting path. *)
    type t =
      | Empty of term
      | Step of t * step

    let empty t = Empty t

    let rec source = function
      | Step (p,_) -> source p
      | Empty t -> t

    let rec target = function
      | Step (_,s) -> Step.target s
      | Empty t -> t

    let step s =
      Step (Empty (Step.source s), s)
 
    let append_step p s =
      assert (eq (target p) (Step.source s));
      Step (p,s)

    let rec to_string ?var = function
      | Empty t -> string_of_term ?var t
      | Step (p,s) ->
        let src = to_string ?var p in
        let lbl = Step.label ?var s in
        let tgt = string_of_term ?var (Step.target s) in
        src ^ " -" ^ lbl ^ "-> " ^ tgt

    let rec append p = function
      | Step (q, s) -> Step (append p q, s)
      | Empty t ->
         assert (eq (target p) t);
         p

    (** Rules which are used without context. *)
    let rec toplevel_rules = function
      | Empty t -> []
      | Step (p,s) -> (toplevel_rules p)@(if Step.has_context s then [] else [Step.rule s])

    let rec eq p p' =
      match p,p' with
      | Step (p,s), Step (p',s') -> eq p p' && Step.eq s s'
      | Empty t, Empty t' -> eq_term t t'
      | _ -> false

    (** Rules occurring in a step. *)
    let rec rules = function
      | Step (p,s) ->
        let r = Step.rule s in
        let rr = rules p in
        if List.exists (Rule.eq r) rr then rr
        else r::rr
      | Empty _ -> []

    (** Length of a path. *)
    let rec length = function
      | Step (p, _) -> 1 + length p
      | Empty _ -> 0

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
  let rec normalize rs t =
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
    aux id (Rule.source r1)

  (** Critical branchings. *)
  let critical (rs:t) =
    let steps = List.flatten (List.flatten (List.map (fun r1 -> List.map (fun r2 -> critical_rules r1 r2) rs) rs)) in
    (* Remove symmetric pairs. *)
    let rec sym = function
      | (s1,s2)::steps ->
         let steps = List.filter (fun (s2',s1') -> not (Step.eq s1 s1' && Step.eq s2 s2')) steps in
         (s1,s2)::(sym steps)
      | [] -> []
    in
    sym steps

  (** Knuth-Bendix completion. [gt] is the strict order on terms, [callback] is
      a function which is called regularly with the current rewriting system as
      argument (useful to display during the completion). *)
  let knuth_bendix?(gt=LPO.gt (>=)) ?(callback=fun _ -> ()) (rs:t) =
    (* Reorient the rules according to the order. *)
    let rs = List.map (fun (n,t,u) -> if not (gt t u) then (n,u,t) else (n,t,u)) rs in
    (* Name for new rules. *)
    let name =
      let n = ref (-1) in
      fun () -> incr n; "K"^string_of_int !n
    in
    (* Rules to handle. *)
    let queue = ref rs in
    (* Produced rewriting system. *)
    let rs = ref rs in
    let add (r:Rule.t) =
      (* Printf.printf "add %s\n%!" (Rule.to_string r); *)
      rs := r :: !rs;
      (* Normalize the rules *)
      rs :=
        List.map
          (fun ((n,s,t) as r) ->
             (* TODO: proper recursive function instead of this filter *)
             let rs = List.filter (fun r' -> not (Rule.eq r r')) !rs in
             n, Path.target (normalize rs s), Path.target (normalize rs t)
          ) !rs;
      rs := List.filter (fun (n,s,t) -> not (eq s t)) !rs;
      queue := !queue@[r]
      (* queue := r :: !queue *)
    in
    while !queue <> [] do
      let r = List.hd !queue in
      queue := List.tl !queue;
      let cp = List.flatten (List.map (fun s -> (critical_rules r s)@(critical_rules s r)) !rs) in
      List.iter
        (fun (s1, s2) ->
           let p1 = Path.append (Path.step s1) (normalize !rs (Step.target s1)) in
           let p2 = Path.append (Path.step s2) (normalize !rs (Step.target s2)) in
           let t1 = Path.target p1 in
           let t2 = Path.target p2 in
           if not (eq t1 t2) then
             let t1, t2 = if gt t1 t2 then t1, t2 else t2, t1 in
             let r = (name (), t1, t2) in
             Printf.printf "add %s\n%s\n%s\n\n%!" (Rule.to_string r) (Path.to_string p1) (Path.to_string p2);
             add r
        ) cp;
      callback !rs
    done;
    !rs

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
    (** A rewriting zigzag. *)
    type t =
      | Step of Step.t
      | Comp of t * t
      | Id of term
      | Inv of t

    (** String representation. *)
    let rec to_string ?(pa=false) ?var = function
      | Step s -> Step.label ?var s
      | Comp (p1,p2) ->
        let s = to_string ~pa:true ?var p1 ^ "." ^ to_string ?var p2 in
        if pa then "(" ^ s ^ ")" else s
      | Id t -> string_of_term ?var t
      | Inv p -> to_string ~pa:true ?var p ^ "-"

    let rec source = function
      | Step s -> Step.source s
      | Comp (p, _) -> source p
      | Id t -> t
      | Inv p -> target p
    and target = function
      | Step s -> Step.target s
      | Comp (_, p) -> target p
      | Id t -> t
      | Inv p -> source p

    (** Path reduced to one step. *)
    let step s = Step s

    (** Concatenation of two paths. *)
    let comp p1 p2 =
      assert (eq (target p1) (source p2));
      Comp (p1, p2)

    let rec append p1 p2 = comp p1 p2

    (** Concatenation of a list of paths. *)
    let rec concat = function
      | [p] -> p
      | p::l -> append p (concat l)
      | [] -> assert false

    (** Inverse of a path. *)
    let inv p = Inv p

    (** Equality between paths. *)
    let rec eq p p' =
      match p, p' with
      | Step s, Step s' -> Step.eq s s'
      | Comp (p, q), Comp (p', q') -> eq p p' && eq q q'
      | Id t, Id t' -> eq_term t t'
      | Inv p, Inv p' -> eq p p'
      | _ -> false

    (** Number of steps in a path. *)
    let rec length = function
      | Step _ -> 1
      | Comp (p, q) -> length p + length q
      | Id _ -> 0
      | Inv p -> length p

    (** Create a zigzag from a path. *)
    let rec of_path p =
      match p with
      | Path.Empty t -> Id t
      | Step (p, s) -> comp (of_path p) (step s)

    (** Apply a context function to a path. In need to have two function because
        of typing issues (variance and polymorphic variants...), but they will
        always be the same in practice. *)
    let rec map tm rs = function
      | Step s -> Step (rs s)
      | Comp (p, q) -> Comp (map tm rs p, map tm rs q)
      | Id t -> Id (tm t)
      | Inv p -> Inv (map tm rs p)

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

    (** Whether a path contains a rule. *)
    let rec has_rule r p = rule_occurences r p > 0

    (** Put path in canonical form. *)
    let rec canonize p =
      (* Printf.printf "canonize: %s\n%!" (to_string p); *)
      match p with
      | Comp (Id _, p) -> canonize p
      | Comp (p, Id _) -> canonize p
      | Comp (Comp (p, q), r) -> canonize (Comp (p, Comp (q, r)))
      | Comp (Step s, p) ->
        (
          match canonize p with
          | Inv (Step s') when Step.eq s s' -> Id (Step.source s)
          | Comp (Inv (Step s'), p) when Step.eq s s' -> p
          | p -> Comp (Step s, p)
        )
      | Comp (Inv (Step s), p) ->
        (
          match canonize p with
          | Step s' when Step.eq s s' -> Id (Step.target s')
          | Comp (Step s', p) when Step.eq s s' -> p
          | p -> Comp (Inv (Step s), p)
        )
      | Comp (p, q) -> canonize (Comp (canonize p, q))
      | Inv (Inv p) -> canonize p
      | Inv (Comp (p, q)) -> canonize (Comp (Inv q, Inv p))
      | Inv (Id t) -> Id t
      | Inv (Step s) -> Inv (Step s)
      | Id t -> Id t
      | Step s -> Step s

    (** Express a rule as a zigzag in a cell. *)
    let rec value r (p1,p2) =
      assert (rule_occurences r p1 + rule_occurences r p2 = 1);
      if not (has_rule r p1) then value r (p2,p1)
      else
        let rec aux prefix = function
          | Comp (Step (RApp (r', s)), p) when Rule.eq r r'->
            assert (not (has_rule r p));
            assert (Subst.is_renaming s);
            let prefix = concat (List.rev prefix) in
            subst (Subst.inv s) (concat [inv prefix; p2; p])
          | Comp (Step _ as s, p) | Comp (Inv (Step _) as s, p) -> aux (s::prefix) p
          | Step _ | Inv (Step _) as s -> aux prefix (Comp (s, Id (target s)))
          | Id _ -> assert false
          | _ -> assert false
        in
        canonize (aux [Id (source p1)] (canonize p1))

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
      let replace_step = replace_step id id in
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

    let rec to_list = function
      | Step s -> [Step s]
      | Comp (p, q) -> (to_list p)@(to_list q)
      | Id _ -> []
      | Inv p -> List.map (fun p -> Inv p) (List.rev (to_list p))
  end

  (** Coherent presentations. *)
  module Coherent = struct
    (** A coherent presentation. *)
    type t =
      {
        operations : Op.t list;
        rules : Rule.t list;
        coherence : (string * (Zigzag.t * Zigzag.t)) list;
      }

    let to_string ?(var=Var.namer) rs =
      let coherence =
        List.map
          (fun (c,(p1,p2)) ->
             let var = var () in
             let p1 = Zigzag.canonize p1 in
             let p2 = Zigzag.canonize p2 in
             Printf.sprintf "%s:\n%s\n%s\n" c (Zigzag.to_string ~var p1) (Zigzag.to_string ~var p2)
          ) rs.coherence
      in
      let coherence = String.concat "\n" coherence in
      coherence

    let to_tex ?(var=Var.namer_natural) rs =
      let ans = ref "" in
      let print s = Printf.ksprintf (fun s -> ans := !ans ^ s) s; in
      print "\\documentclass{article}\n\
             \\usepackage[utf8x]{inputenc}\n\
             \\usepackage{amsmath}\n\
             \\usepackage{tikz-cd}\n\
             \\usepackage{a4wide}\n\n\
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
          ) rs.rules
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
        (fun (c,(p1,p2)) ->
           let p1 = Zigzag.canonize p1 in
           let p2 = Zigzag.canonize p2 in
           let p1,p2 = if Zigzag.length p1 > Zigzag.length p2 then p2,p1 else p1,p2 in
           let var = var () in
           let st n p =
             let d, s = Zigzag.nth_step n p in
             let d = if d then "" else "<-," in
             let s = Step.label ~var s in
             d ^ "\"{" ^ s ^ "}\""
           in
           let tm n p = string_of_term ~var (Zigzag.nth_term n p) in
           let cd =
             match Zigzag.length p1, Zigzag.length p2 with
             | 1, 1 ->
               Printf.sprintf "%s\\ar[d,bend right,%s']\\ar[d,bend left,%s]\\\\\n%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p1)
             | 1, 2 ->
               Printf.sprintf "%s\\ar[dr,%s']\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2)
             | 1, 3 ->
               Printf.sprintf "%s\\ar[drr,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2)
             | 1, 4 ->
               Printf.sprintf "%s\\ar[ddrr,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2)
             | 1, 5 ->
               Printf.sprintf "%s\\ar[dddrr,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2)
             | 1, 6 ->
               Printf.sprintf "%s\\ar[ddddrr,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s\\ar[d,%s]\\\\\n&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2) (st 5 p2) (tm 6 p2)
             | 2, 2 ->
               Printf.sprintf "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[d,%s]\\\\\n%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 1 p1) (st 1 p1) (tm 2 p1)
             | 2, 3 ->
               Printf.sprintf "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n%s\\ar[rr,%s']&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 1 p1) (st 1 p1) (tm 2 p1)
             | 2, 4 ->
               Printf.sprintf "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n%s\\ar[rrr,%s']&&&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 1 p1) (st 1 p1) (tm 2 p1)
             | 3, 3 ->
               Printf.sprintf "%s\\ar[d,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 4 ->
               Printf.sprintf "%s\\ar[dd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 5 ->
               Printf.sprintf "%s\\ar[ddd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 8 ->
               Printf.sprintf "%s\\ar[dddddd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2) (st 5 p2) (tm 6 p2) (st 6 p2) (tm 7 p2) (st 7 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 9 ->
               Printf.sprintf "%s\\ar[ddddddd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2) (st 5 p2) (tm 6 p2) (st 6 p2) (tm 7 p2) (st 7 p2) (tm 8 p2) (st 8 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | 3, 10 ->
               Printf.sprintf "%s\\ar[dddddddd,%s']\\ar[r,%s]&%s\\ar[r,%s]&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               &&%s\\ar[d,%s]\\\\\n\
                               %s\\ar[r,%s']&%s\\ar[r,%s']&%s"
                 (tm 0 p1) (st 0 p1) (st 0 p2) (tm 1 p2) (st 1 p2) (tm 2 p2) (st 2 p2) (tm 3 p2) (st 3 p2) (tm 4 p2) (st 4 p2) (tm 5 p2) (st 5 p2) (tm 6 p2) (st 6 p2) (tm 7 p2) (st 7 p2) (tm 8 p2) (st 8 p2) (tm 9 p2) (st 9 p2) (tm 1 p1) (st 1 p1) (tm 2 p1) (st 2 p1) (tm 3 p1)
             | l1, l2 ->
               let p = Zigzag.canonize (Zigzag.append p1 (Zigzag.inv p2)) in
               let l = Zigzag.length p in
               let n = 2 in
               let ans = ref "" in
               for i = 0 to (l-1)/n do
                 ans := !ans ^ (tm (i*n) p);
                 for j = 0 to min n (l - i*n) - 1 do
                   ans := Printf.sprintf "%s\\ar[r,%s]&%s" !ans (st i p) (tm (i*n+j+1) p)
                 done;
                 ans := !ans ^ "\\\\"
               done;
               !ans
               (* Printf.sprintf "TODO: %d, %d" l1 l2 *)
           in
           print "\\noindent\n\\subsection*{%s}\n" c;
           print "\\[\n\\begin{tikzcd}\n%s\n\\end{tikzcd}\n\\]\n\n" cd
        ) rs.coherence;
      print "\\end{document}\n";
      !ans

    let view_pdf ?var rs =
      let fname, oc = Filename.open_temp_file "ocaml-alg" ".tex" in
      output_string oc (to_tex ?var rs);
      close_out oc;
      let cmd = Printf.sprintf "cd %s && pdflatex %s && evince `basename %s .tex`.pdf" (Filename.get_temp_dir_name ()) fname fname in
      assert (Sys.command cmd = 0)

    let make operations rules coherence =
      { operations; rules; coherence }

    let find_rule rs r =
      find_rule rs.rules r

    let find_coherence rs c =
      List.assoc c rs.coherence

    (** Eliminate a rule with a coherence. *)
    let elim_rule rs r c =
      let r = find_rule rs r in
      (* let cname = c in *)
      let c = find_coherence rs c in
      let v = Zigzag.value r c in
      let var = Var.namer_natural () in Printf.printf "\nelim rule: [%s] => %s\n%!" (Rule.to_string ~var r) (Zigzag.to_string ~var v);
      let rules = List.filter (fun r' -> not (Rule.eq r r')) rs.rules in
      let coherence = List.map (fun (c,(p1,p2)) -> c, (Zigzag.replace_rule r v p1, Zigzag.replace_rule r v p2)) rs.coherence in
      (* let coherence = List.filter (fun (c,_) -> c <> cname) coherence in *)
      { rs with rules; coherence }
  end
end
