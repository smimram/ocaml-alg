(** Term rewriting systems. *)

(** Operations. *)
module Op = struct
  (** An operation. *)
  type t = string * int

  let make name arity : t =
    (name, arity)

  (* TODO: it would be even better to use physical equality *)
  let eq (g1:t) (g2:t) = (g1 = g2)

  let to_string (g:t) = fst g

  let arity (g:t) = snd g
end

(** Variables. *)
module Var = struct
  (** A variable. *)
  type t = unit ref

  let fresh () : t = ref ()

  let eq (x:t) (y:t) = x == y

  let to_string =
    let cur = ref 0 in
    let nn = ref [] in
    fun (v:t) ->
    let n =
      try
        List.assq v !nn
      with
      | Not_found ->
         let n = !cur in
         incr cur;
         nn := (v,n) :: !nn;
         n
    in
    "x" ^ string_of_int n
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
       if l1 <> Array.length a2 then false else
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
let rec to_string = function
  | App (f, a) -> Op.to_string f ^ "(" ^ String.concat "," (List.map to_string (Array.to_list a)) ^ ")"
  | Var x -> Var.to_string x

let string_of_term = to_string

(** Is a term a variable? *)
let is_var = function
  | Var _ -> true
  | _ -> false

(** Whether a variable occurs in a term. *)
let rec occurs x = function
  | App (_, a) -> Array.exists (occurs x) a
  | Var y -> Var.eq x y

(** Substitutions. *)
module Substitution = struct
  (** A substitution. *)
  type t = (var * term) list

  let to_string s =
    "[" ^ String.concat "," (List.map (fun (x,t) -> to_string t ^ "/" ^ to_string (Var x)) s) ^ "]"

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
let equivalent t1 t2 =
  let rec aux q s =
    match q with
    | [] -> s
    | p::q ->
       match p with
       | Var x, Var y ->
          Subst.add s x (Var y)
       | _, Var _ | Var _, _ -> raise Not_unifiable
       | App (f1,a1), App (f2,a2) ->
          if not (Op.eq f1 f2) then raise Not_unifiable;
          let q = ref q in
          for i = 0 to Array.length a1 - 1 do
            q := (a1.(i),a2.(i)) :: !q
          done;
          aux !q s
  in
  aux [t1,t2] Subst.empty

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

    let eq (r1:t) (r2:t) =
      if name r1 <> name r2 then false else
        try
          let s = equivalent (source r1) (source r2) in
          eq (Subst.app s (target r1)) (target r2)
        with
        | Not_unifiable -> false

    let to_string r =
      name r ^ " : " ^ to_string (source r) ^ " -> " ^ to_string (target r)

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

    let rule_name s = Rule.name (rule s)

    let subst ((t,p,r,s):t) = s

    let eq (s1:t) (s2:t) =
      eq (source s1) (source s2)
      && Pos.eq (pos s1) (pos s2)
      && Rule.eq (rule s1) (rule s2)
      && Subst.eq (subst s1) (subst s2)

    let to_string s =
      to_string (source s) ^ " -" ^ rule_name s ^ "@" ^ Pos.to_string (pos s) ^ "-> " ^ string_of_term (target s)
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

    let rec to_string = function
      | Empty t -> string_of_term t
      | Step (p,s) -> to_string p ^ " -" ^ Step.rule_name s ^ "-> " ^ string_of_term (Step.target s)
  end

  (** Normalize a term. *)
  let rec normalize rs t =
    let rec aux p =
      let s = steps rs (Path.target p) in
      if s = [] then p else aux (Path.step p (List.hd s))
    in
    aux (Path.empty t)

  (** Critical branchings. *)
  let critical (rs:t) =
    (* Unify r2 with a subterm of r1, p is the position in the left member of r1
       and t is the corresponding term. *)
    let critical_rules r1 r2 =
      let r2 = Rule.refresh r2 in
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
    in
    let steps = List.flatten (List.flatten (List.map (fun r1 -> List.map (fun r2 -> critical_rules r1 r2) rs) rs)) in
    (* Remove symmetric pairs. *)
    let rec sym = function
      | (s1,s2)::steps ->
         let steps = List.filter (fun (s2',s1') -> not (Step.eq s1 s1' && Step.eq s2 s2')) steps in
         (s1,s2)::(sym steps)
      | [] -> []
    in
    sym steps
end
