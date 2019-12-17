(** Term rewriting systems. *)

open Extlib

(** Operations. *)
module Op = struct
  (** An operation. *)
  type t = int * string * int

  (** Create an operation with given name and arity. *)
  let make ?(weight=0) name arity : t =
    (weight, name, arity)

  (** Compare two operations for equality. *)
  (* TODO: it would be even better to use physical equality *)
  let eq (g1:t) (g2:t) = (g1 = g2)

  let name (_,name,_:t) = name

  let to_string f = name f

  let arity (_,_,n:t) = n
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
  | App of Op.t * t array (** application *)
  | Var of var (** variable *)

type term = t

(** Create a fresh variable term. *)
let var () = Var (Var.fresh ())

(** Create an application. *)
let app f a : t =
  assert (Array.length a = Op.arity f);
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
      let a = Array.of_list a in
      App (f, a)
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
  | App (f1, _), App (f2, _) when not (Op.eq f1 f2) -> false
  | App (_, a1), App (_, a2) ->
    (
      let l1 = Array.length a1 in
      if l1 <> Array.length a2 then assert false;
      try
        for i = 0 to Array.length a1 - 1 do
          if not (eq a1.(i) a2.(i)) then raise Exit
        done;
        true
      with
      | Exit -> false
    )
  | _ -> false

(** String representation of a term. *)
let rec to_string ?(var=Var.to_string) = function
  | App (f, a) -> Op.to_string f ^ "(" ^ String.concat "," (List.map (to_string ~var) (Array.to_list a)) ^ ")"
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
      Array.fold_left (fun vars t -> aux vars t) vars a
    | Var x ->
      if List.exists (Var.eq x) vars then vars
      else x::vars
  in
  List.rev (aux [] t)

(** Whether a variable occurs in a term. *)
let rec occurs x = function
  | App (_, a) -> Array.exists (occurs x) a
  | Var y -> Var.eq x y

(** Lexicographic path order on terms. *)
module LPO = struct
  let rec gt ge_op t u =
    (* Printf.printf "%s > %s ?\n%!" (to_string t) (to_string u); *)
    match t, u with
    | t, Var x -> not (eq t u) && occurs x t
    | App (f,a), App (g, b) ->
      if Array.exists (fun t -> ge ge_op t u) a then true else
      if Op.eq f g then
        (* lexicographic > *)
        let rec lex l1 l2 =
          match l1, l2 with
          | x1::l1, x2::l2 when eq x1 x2 -> lex l1 l2
          | x1::l1, x2::l2 when gt ge_op x1 x2 -> true
          | _, _ -> false
        in
        Array.for_all (fun u -> gt ge_op t u) b &&
        lex (Array.to_list a) (Array.to_list b)
      else if ge_op f g then Array.for_all (fun u -> gt ge_op t u) b
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
    | App (g, a) -> App (g, Array.map (app s) a)
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

  (** Inverse of a renaming. *)
  let inv s : t =
    List.map
      (fun (x,t) ->
         match t with
         | Var y -> y, Var x
         | _ -> assert false
      ) s

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
  let inverse (s:t) =
    List.map
      (fun (x,t) ->
        match t with
        | Var y -> y, Var x
        | _ -> failwith "Not inversible."
      ) s

  let in_dom (s:t) x =
    List.exists (fun (y,_) -> Var.eq y x) s

  (** Restrict the domain of a substitution. *)
  let restrict vars s : t =
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
          let q = ref q in
          (* TODO: downto would be more natural *)
          for i = 0 to Array.length a1 - 1 do
            q := (a1.(i),a2.(i)) :: !q
          done;
          aux !q s
  in
  aux [t1,t2] Subst.empty

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
        let q = ref q in
        for i = 0 to Array.length a1 - 1 do
          q := (a1.(i),a2.(i)) :: !q
        done;
        aux !q s
  in
  aux [t1,t2] Subst.empty

(*
let matches t1 t2 =
  let s = matches t1 t2 in
  Printf.printf "MATCH %s WITH %s IS %s\n%!" (to_string t1) (to_string t2) (Subst.to_string s);
  s
 *)

(** Whether two terms are alpha-equivalent *)
let equivalent ?(s=Subst.empty) t1 t2 =
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
        let q = ref q in
        for i = 0 to Array.length a1 - 1 do
          q := (a1.(i),a2.(i)) :: !q
        done;
        aux !q s
  in
  aux [t1,t2] s

(** Positions in terms. *)
module Pos = struct
  (** A position in a term. *)
  type t = int list

  (** Initial position. *)
  let empty : t = []

  let to_string (p:t) =
    "[" ^ String.concat ":" (List.map string_of_int p) ^ "]"

  let is_empty (p:t) = p = []

  (** Append a direction. *)
  let append (p:t) i : t = p@[i]

  let rec subterm t (p:t) =
    match t, p with
    | App(f,a), i::p -> subterm a.(i) p
    | t, [] -> t
    | _ -> failwith "Bad position."

  let eq (p1:t) (p2:t) = p1 = p2
end

type pos = Pos.t

(** Rewriting systems. *)
module RS = struct
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
    type t = term * pos * rule * subst

    (** Create a rewriting step. *)
    let make t p r s : t =
      (* Printf.printf "STEP : %s / %s / %s / %s\n%!" (string_of_term t) (Pos.to_string p) (Rule.to_string r) (Subst.to_string s); *)
      assert (eq (Pos.subterm t p) (Subst.app s (Rule.source r)));
      t,p,r,s

    (** Source. *)
    let source ((t,p,r,s):t) = t

    (** Target. *)
    let rec target ((t,p,r,s):t) =
      match p with
      | i::p ->
        (
          match t with
          | App (f,a) ->
            let a = Array.copy a in
            a.(i) <- target (a.(i),p,r,s);
            App (f,a)
          | _ -> failwith "Bad rewriting step."
        )
      | [] -> Subst.app s (Rule.target r)

    let pos ((t,p,r,s):t) = p

    let rule ((t,p,r,s):t) = r

    let subst ((t,p,r,s):t) = s

    let has_context s =
      not (Pos.is_empty (pos s) && Subst.is_injective_renaming (subst s))

    (* TOOD: remove this *)
    let oldlabel ?var s = Pos.to_string (pos s) ^ Rule.name (rule s) ^ Subst.to_string ?var (subst s)

    let rec label ?var s =
      (* Printf.printf "label: %s\n%!" (oldlabel ?var s); *)
      (* Printf.printf "rule : %s\n%!" (Rule.to_string ?var (rule s)); *)
      match pos s, source s with
      | p::pos, App (f, a) ->
        let ap = label ?var (a.(p), pos, rule s, subst s) in
        let a = Array.map (string_of_term ?var) a in
        a.(p) <- ap;
        let a = String.concat "," (Array.to_list a) in
        Op.to_string f ^ "(" ^ a ^ ")"
      | p::pos, Var x -> assert false
      | [], _ ->
        let r = rule s in
        let args = Rule.args r (subst s) in
        let args = List.map (string_of_term ?var) args in
        let args = String.concat "," args in
        Rule.name r ^ "(" ^ args ^ ")"

    let to_string ?var s =
      string_of_term ?var (source s) ^ " -" ^ label ?var s ^ "-> " ^ string_of_term ?var (target s)

    let eq (s1:t) (s2:t) =
      (* Printf.printf "EQ %s WITH %s\n%!" (to_string s1) (to_string s2); *)
      try
        let _ = equivalent (source s1) (source s2) in
        (* TODO: is it ok not to propagate the substitution to the rules? *)
        Pos.eq (pos s1) (pos s2)
        && Rule.eq (rule s1) (rule s2)
        && Subst.eq (subst s1) (subst s2)
      with
      | Not_unifiable -> false
  end

  type step = Step.t

  (** All possible rewriting steps. *)
  let steps (rs:t) t : step list =
    let t0 = t in
    let rec aux r p = function
      | Var x -> []
      | App (f,a) as t ->
         let s =
           try
             let s = matches (Rule.source r) t in
             [Step.make t0 p r s]
           with
           | Not_unifiable -> []
         in
         let s = ref s in
         for i = 0 to Array.length a - 1 do
           let p = Pos.append p i in
           s := !s @ (aux r p a.(i))
         done;
         !s
    in
    List.flatten (List.map (fun r -> aux r Pos.empty t) rs)

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

    let step p s =
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

    let term_eq = eq
    let rec eq p p' =
      match p,p' with
      | Step (p,s), Step (p',s') -> eq p p' && Step.eq s s'
      | Empty t, Empty t' -> term_eq t t'
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
      if s = [] then p else aux (Path.step p (List.hd s))
    in
    aux (Path.empty t)

  (** Unify the source of [r2] with a subterm of the source of [r1]. *)
  let critical_rules r1 r2 =
    (* p is the position in the left member of r1 and t is the corresponding
       term *)
    let rec aux p t =
      match t with
      | Var x -> []
      | App (f,a) ->
        let s =
          try
            let t2 = Rule.source r2 in
            let n1 = Subst.rename (vars t) in
            let n2 = Subst.rename (vars t2) in
            let s = unify (Subst.app n1 t) (Subst.app n2 t2) in
            let s1 = Subst.compose n1 s in
            let s2 = Subst.compose n2 s in
            let t = Subst.app s1 (Rule.source r1) in
            let step1 = Step.make t Pos.empty r1 s1 in
            let step2 = Step.make t p r2 s2 in
            if Pos.is_empty p && Rule.eq r1 r2 then [] else [step1,step2]
          with
          | Not_unifiable -> []
        in
        let s = ref s in
        for i = 0 to Array.length a - 1 do
          let p = Pos.append p i in
          s := !s @ (aux p a.(i))
        done;
        !s
    in
    aux Pos.empty (Rule.source r1)

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
           let p1 = Path.append (Path.step (Path.empty (Step.source s1)) s1) (normalize !rs (Step.target s1)) in
           let p2 = Path.append (Path.step (Path.empty (Step.source s2)) s2) (normalize !rs (Step.target s2)) in
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
        let p1 = Path.append (Path.step (Path.empty (Step.source s1)) s1) (normalize rs (Step.target s1)) in
        let p2 = Path.append (Path.step (Path.empty (Step.source s2)) s2) (normalize rs (Step.target s2)) in
        if not (eq (Path.target p1) (Path.target p2)) then raise Not_confluent;
        p1, p2
      )
      (critical rs)

  (** Rewriting zigzags. *)
  module Zigzag = struct
    (** Zigzag step. *)
    (* We change the representation to more traditional terms, because we want
       to be able to rewrite. *)
    module Step = struct
      type t =
        | TApp of Op.t * t list (** Term application. *)
        | RApp of Rule.t * Subst.t (** Rule application. *)
        | SVar of var (** Variable. *)

      let rec to_string ?(var=Var.to_string) = function
        | TApp (f, a) ->
          let a = List.map (to_string ~var) a in
          let a = String.concat "," a in
          Op.name f ^ "(" ^ a ^ ")"
        | RApp (r, s) ->
          let a = Rule.args r s in
          let a = List.map (string_of_term ~var) a in
          let a = String.concat "," a in
          Rule.name r ^ "(" ^ a ^ ")"
        | SVar x -> var x

      (* let of_string ops vars s = *)
        (* let s = parser s in *)
        (* let rec aux = function *)
          (* | `Seq l ->  *)
        (* in *)

      let rec of_term = function
        | App (f, a) -> TApp (f, List.map of_term (Array.to_list a))
        | Var x -> SVar x

      let rec of_step ((t,pos,r,s):Step.t) =
        match t, pos with
        | _, [] ->
          RApp (r, s)
        | App (f, a), p::pos ->
          let t = a.(p) in
          let a = Array.map of_term a in
          a.(p) <- of_step (t, pos, r, s);
          let a = Array.to_list a in
          TApp (f, a)
        | _ -> assert false

      let rec eq s1 s2 =
        match s1,s2 with
        | TApp (f, a), TApp (f', a') -> Op.eq f f' && List.for_all2 eq a a'
        | RApp (r, s), RApp (r', s') -> Rule.eq r r' && Subst.eq s s'
        | SVar x, SVar y -> Var.eq x y
        | _ -> false

      (** Apply a substitution. *)
      let rec subst s = function
        | TApp (f, a) -> TApp (f, List.map (subst s) a)
        | RApp (r, s') -> RApp (r, Subst.compose s' s)
        | SVar x -> of_term (Subst.find s x)

      let rec source = function
        | TApp (f, a) -> TApp (f, List.map source a)
        | RApp (r, s) -> subst s (of_term (Rule.source r))
        | SVar x -> SVar x

      let rec target = function
        | TApp (f, a) -> TApp (f, List.map target a)
        | RApp (r, s) ->
          Printf.printf "rule is %s\n%!" (Rule.to_string r);
          Printf.printf "subst is %s\n%!" (Subst.to_string s);
          subst s (of_term (Rule.target r))
        | SVar x -> SVar x
      let target t =
        Printf.printf "taget: %s\n%!" (to_string t);
        target t

      (** Whether a rule occurs in a step. *)
      let rec has_rule r = function
        | TApp (_, a) -> List.exists (has_rule r) a
        | RApp (r', _) -> Rule.eq r r'
        | SVar _ -> false
    end

    (** A rewriting zigzag. *)
    type t =
      | Step of bool * Step.t * t (* false means inverted *)
      | Empty of Step.t (* term encoded as a step for compatibility with contexts *)

    (** String representation. *)
    let to_string ?var p =
      match p with
      | Empty t -> Step.to_string ?var t
      | _ ->
        let rec aux = function
          | Step (d,s,Empty _) ->
            Step.to_string ?var s ^ (if d then "" else "-")
          | Step (d,s,p) ->
            Step.to_string ?var s ^ (if d then "" else "-") ^ "." ^ aux p
          | Empty _ -> assert false
        in
        aux p

    (** Create a zigzag from a path. *)
    let rec of_path p =
      match p with
      | Path.Empty t -> Empty (Step.of_term t)
      | _ ->
        (* Compute the list of steps. *)
        let rec aux = function
          | Path.Step (p,s) -> s::(aux p)
          | Empty _ -> []
        in
        let l = List.rev (aux p) in
        let rec aux = function
          | s::l -> Step (true, Step.of_step s, aux l)
          | [] -> Empty (Step.of_term (Path.target p))
        in
        aux l

    let source = function
      | Step (d, s, _) -> (if d then Step.source else Step.target) s
      | Empty t -> t

    let rec target = function
      | Step (_, _, p) -> target p
      | Empty t -> t

    (** Path reduced to one step. *)
    let step d s =
      (* Printf.printf "making step %s %s (target is %s)\n%!" (if d then "+" else "-") (Step.to_string s) (Step.to_string (Step.target s)); *)
      if d then Step (d, s, Empty (Step.target s))
      else Step (d, s, Empty (Step.source s))

    (** Whether a path is empty. *)
    let is_empty = function
      | Step _ -> false
      | Empty _ -> true

    let rec to_list = function
      | Step (d, s, p) -> (d,s)::(to_list p)
      | Empty t -> []

    let rec of_list = function
      | [d,s] -> step d s
      | (d,s)::l -> Step (d, s, of_list l)
      | [] -> assert false

    (** Concatenation of two paths. *)
    let rec append p1 p2 =
      (* Printf.printf "append %s with %s\n%!" (to_string p1) (to_string p2); *)
      match p1 with
      | Step (d, s, p) -> Step (d, s, append p p2)
      | Empty t ->
        assert (Step.eq (source p2) t);
        p2

    (** Inverse of a path. *)
    let inv p =
      match p with
      | Empty t -> Empty t
      | _ ->
        p |> to_list |> List.map (fun (d,s) -> not d, s) |> List.rev |> of_list

    (** Apply a substitution. *)
    let rec subst s = function
      | Step (d, st, p) -> Step (d, Step.subst s st, subst s p)
      | Empty t -> Empty (Step.subst s t)

    (** Number of occurences of a given rule in a path. *)
    let rec rule_occurences r = function
      | Step (_, s, p) ->
        (if Step.has_rule r s then 1 else 0) + rule_occurences r p
      | Empty _ -> 0

    (** Whether a path contains a rule. *)
    let rec has_rule r = function
      | Step (_, s, p) -> Step.has_rule r s || has_rule r p
      | Empty _ -> false

    (** Express a rule as a zigzag in a cell. *)
    let rec value r (p1,p2) =
      assert (rule_occurences r p1 + rule_occurences r p2 = 1);
      if not (has_rule r p1) then value r (p2,p1)
      else
        let rec aux prefix = function
          | Step (d, (RApp (r', s)), p) when Rule.eq r r' ->
            assert (not (has_rule r p));
            assert (Subst.is_renaming s);
            let prefix = List.rev prefix in
            let prefix = if prefix = [] then Empty (source p2) else of_list prefix in
            subst (Subst.inv s) (append (append (inv prefix) p2) p)
          | Step (d, s, p) -> aux ((d,s)::prefix) p
          | Empty _ -> assert false
        in
        aux [] p1

    let rec map f = function
      | Step (d, s, p) -> Step (d, f s, map f p)
      | Empty t -> Empty (f t)

    (** Replace a rule by a path in a path. *)
    let rec replace_rule r (pr:t) p =
      Printf.printf "replace_rule: %s\n%!" (to_string p);
      let rec replace_step ctx = function
        | Step.TApp (f, a) ->
          let n = List.index (Step.has_rule r) a in
          let t = List.nth a n in
          let ctx t = ctx (Step.TApp (f, List.replace_nth a n t)) in
          replace_step ctx t
        | RApp (r', s) when Rule.eq r r' -> map ctx (subst s pr)
        | RApp (r, s) -> assert false
        | SVar x -> assert false
      in
      let replace_step d s =
        if Step.has_rule r s then
          let ans = replace_step id s in
          if d then ans else inv ans
        else step d s
      in
      let replace_step d s =
        Printf.printf "replace_step: %s%s\n%!" (if d then "" else "-") (Step.to_string s);
        let ans = replace_step d s in
        Printf.printf "replace_step: %s%s is %s\n%!" (if d then "" else "-") (Step.to_string s) (to_string ans);
        ans
      in
      match p with
      | Step (d, s, p) -> append (replace_step d s) (replace_rule r pr p)
      | Empty t -> Empty t
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

    let make operations rules coherence =
      { operations; rules; coherence }

    let find_rule rs r =
      find_rule rs.rules r

    let find_coherence rs c =
      List.assoc c rs.coherence

    (** Eliminate a rule with a coherence. *)
    let elim_rule rs r c =
      let r = find_rule rs r in
      let c = find_coherence rs c in
      let v = Zigzag.value r c in
      let var = Var.namer_natural () in Printf.printf "\nelim rule: [%s] => %s\n%!" (Rule.to_string ~var r) (Zigzag.to_string ~var v);
      let rules = List.filter (fun r' -> not (Rule.eq r r')) rs.rules in
      let coherence = List.map (fun (c,(p1,p2)) -> c, (Zigzag.replace_rule r v p1, Zigzag.replace_rule r v p2)) rs.coherence in
      { rs with rules; coherence }
      (* TODO: remove leftover rule and coherence. *)
  end
end
