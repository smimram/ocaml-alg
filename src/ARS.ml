(** Abstract rewriting systems. *)

(** Signature for specifying rewriting steps. *)
module type Span = sig
  type obj
  
  type t

  val source : t -> obj

  val target : t -> obj

  val eq : t -> t -> bool

  val to_string : t -> string
end

(** Create an abstract rewriting system with given objects and rewriting
    steps. *)
module Make (Obj : Alphabet.T) (Step : Span with type obj = Obj.t) = struct
  (** Rewriting paths. *)
  module Path = struct
    type t =
      | Empty of Obj.t
      | Step of t * Step.t

    let empty u = Empty u

    let step s = Step (Empty (Step.source s), s)

    let rec source = function
      | Empty u -> u
      | Step (p, _) -> source p

    let target = function
      | Empty u -> u
      | Step (_, r) -> Step.target r

    let rec to_string = function
      | Empty u -> Obj.to_string u
      | Step (p, s) -> Printf.sprintf "%s -%s→ %s" (to_string p) (Step.to_string s) (Obj.to_string (Step.target s))

    let rec eq p q =
      match p, q with
      | Empty u, Empty u' -> Obj.eq u u'
      | Step (p, s), Step (p', s') -> eq p p' && Step.eq s s'
      | _ -> false

    let append_step p s =
      assert (Obj.eq (target p) (Step.source s));
      Step (p,s)

    let rec append p = function
      | Step (q, s) -> Step (append p q, s)
      | Empty t ->
         assert (Obj.eq (target p) t);
         p
    
    (** Length of a path. *)
    let rec length = function
      | Step (p, _) -> 1 + length p
      | Empty _ -> 0
  end

  (** Rewriting zigzags. *)
  module Zigzag = struct
    (** A rewriting zigzag. *)
    type t =
      | Step of Step.t
      | Comp of t * t
      | Id of Obj.t
      | Inv of t

    (** String representation. *)
    let rec to_string = function
      | Step s -> Step.to_string s
      | Comp (p1,p2) -> "(" ^ to_string p1 ^ "." ^ to_string p2 ^ ")"
      | Id t -> Obj.to_string t
      | Inv p -> "(" ^ to_string p ^ ")-"

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

    (** Equality between paths. *)
    let rec eq p p' =
      match p, p' with
      | Step s, Step s' -> Step.eq s s'
      | Comp (p, q), Comp (p', q') -> eq p p' && eq q q'
      | Id t, Id t' -> Obj.eq t t'
      | Inv p, Inv p' -> eq p p'
      | _ -> false

    (** Number of steps in a path. *)
    let rec length = function
      | Step _ -> 1
      | Comp (p, q) -> length p + length q
      | Id _ -> 0
      | Inv p -> length p

    (** Path reduced to one step. *)
    let step s = Step s

    (** Concatenation of two paths. *)
    let append p1 p2 =
      (* Printf.printf "compose %s with %s\n%!" (to_string p1) (to_string p2); *)
      (* Printf.printf "%s vs %s\n%!" (string_of_term (target p1)) (string_of_term (source p2)); *)
      assert (Obj.eq (target p1) (source p2));
      Comp (p1, p2)

    let comp = append
        
    (** Concatenation of a list of paths. *)
    let rec concat = function
      | [p] -> p
      | p::l -> append p (concat l)
      | [] -> assert false

    (** Inverse of a path. *)
    let inv p = Inv p

    (** Create a zigzag from a path. *)
    let rec of_path p =
      match p with
      | Path.Empty t -> Id t
      | Step (p, s) -> append (of_path p) (step s)
                         
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
          | Id _ -> Step s
          | p -> Comp (Step s, p)
        )
      | Comp (Inv (Step s), p) ->
        (
          match canonize p with
          | Step s' when Step.eq s s' -> Id (Step.target s')
          | Comp (Step s', p) when Step.eq s s' -> p
          | Id _ -> Inv (Step s)
          | p -> Comp (Inv (Step s), p)
        )
      | Comp (p, q) -> canonize (Comp (canonize p, q))
      | Inv (Inv p) -> canonize p
      | Inv (Comp (p, q)) -> canonize (Comp (Inv q, Inv p))
      | Inv (Id t) -> Id t
      | Inv (Step s) -> Inv (Step s)
      | Id t -> Id t
      | Step s -> Step s
                    
    (** Apply a context function to a path. We need to have two function because
        of typing issues (variance and polymorphic variants...), but they will
        always be the same in practice. *)
    let rec map tm rs = function
      | Step s -> Step (rs s)
      | Comp (p, q) -> Comp (map tm rs p, map tm rs q)
      | Id t -> Id (tm t)
      | Inv p -> Inv (map tm rs p)

    let is_id = function
      | Id _ -> true
      | _ -> false

    let is_inv = function
      | Inv _ -> true
      | _ -> false
        
    (** List.of_steps in a path. *)
    let rec to_list = function
      | Step s -> [Step s]
      | Comp (p, q) -> (to_list p)@(to_list q)
      | Id _ -> []
      | Inv p -> List.map (fun p -> Inv p) (List.rev (to_list p))
  end
end
