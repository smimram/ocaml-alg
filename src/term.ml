(** Term rewriting systems. *)

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
       (
         try
           find s x
         with
           Not_found -> Var x
       )

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
  let inverse (s:t) =
    List.map
      (fun (x,t) ->
        match t with
        | Var y -> y, Var x
        | _ -> failwith "Not inversible."
      ) s

  let in_dom (s:t) x =
    List.exists (fun (y,_) -> Var.eq y x) s
end

module Subst = Substitution

type subst = Subst.t

(** Refresh variables of a term. *)
let rec refresh s t =
  match t with
  | App (f,a) -> App (f, Array.map (refresh s) a)
  | Var x ->
     (
       try
         Subst.find !s x
       with
       | Not_found ->
          let x' = var () in
          s := Subst.add !s x x';
          x'
     )

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
          let xt = Subst.simple x t in
          let fxt = Subst.app xt in
          let q = List.map (fun (t1,t2) -> fxt t1, fxt t2) q in
          let s = Subst.compose s xt in
          aux q (Subst.add s x t)
       | t, Var x ->
          let q = (Var x, t)::q in
          aux q s
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
let unify t1 t2 =
  let s = unify t1 t2 in
  Printf.printf "UNIFY %s WITH %s IS %s\n%!" (to_string t1) (to_string t2) (Subst.to_string s);
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

    let name ((r,s,t):t) = r

    let source ((r,s,t):t) = s

    let target ((r,s,t):t) = t

    let to_string ?var r =
      name r ^ " : " ^ to_string ?var (source r) ^ " -> " ^ to_string ?var (target r)

    let eq (r1:t) (r2:t) =
      (* Printf.printf "EQ %s WITH %s\n%!" (to_string r1) (to_string r2); *)
      if name r1 <> name r2 then false else
        try
          let s = equivalent (source r1) (source r2) in
          eq (Subst.app s (target r1)) (target r2)
        with
        | Not_unifiable -> false

    let refresh ((r,s,t):t) =
      let ss = ref Subst.empty in
      let s = refresh ss s in
      let t = refresh ss t in
      make r s t
  end

  type rule = Rule.t

  (** A rewriting system. *)
  type t = rule list

  (** Empty rewriting system. *)
  let empty : t = []

  let to_string rs =
    String.concat "\n" (List.map (fun r -> Rule.to_string ~var:(Var.namer()) r) rs)

  (** Rewriting steps. *)
  module Step = struct
    (** A rewriting step. *)
    type t = term * pos * rule * subst

    let make t p r s : t =
      (* Printf.printf "STEP : %s / %s / %s / %s\n%!" (to_string t) (Pos.to_string p) (Rule.to_string r) (Subst.to_string s); *)
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

    (* let label ?var s = Pos.to_string (pos s) ^ Rule.name (rule s) ^ Subst.to_string ?var (subst s) *)
    let rec label ?var s =
      match pos s, source s with
      | p::pos, App (f, a) ->
        let ap = label ?var (a.(p), pos, rule s, subst s) in
        let a = Array.map (string_of_term ?var) a in
        a.(p) <- ap;
        let a = String.concat "," (Array.to_list a) in
        Op.to_string f ^ "(" ^ a ^ ")"
      | p::pos, Var x -> assert false
      | [], _ -> Rule.name (rule s) ^ Subst.to_string ?var (subst s)

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
      | Step (p,s) -> to_string ?var p ^ " -" ^ Step.label ?var s ^ "-> " ^ string_of_term ?var (Step.target s)

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
    let r2 = Rule.refresh r2 in
    (* p is the position in the left member of r1 and t is the corresponding
       term *)
    let rec aux p t =
      match t with
      | Var x -> []
      | App (f,a) ->
        let s =
          try
            let s = unify (Rule.source r2) t in
            let t = Subst.app s (Rule.source r1) in
            let step1 = Step.make t Pos.empty r1 s in
            let step2 = Step.make t p r2 s in
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
      fun () -> incr n; "kb"^string_of_int !n
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
end
