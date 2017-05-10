(** Algebras. *)

(** An algebra. *)
module type T = sig
  (** An element of the algebra. *)
  type t

  (** An element of the ring. *)
  type r

  include Ring.T with type t := t

  module Field : sig
    include Field.T with type t := r
  end

  val cmul : r -> t -> t
end

(** Free algebra of a monoid over a ring. *)
module Free (K : Field.T) (M : Monoid.T) = struct
  include Module.FreeLeft(K)(M)

  module Field = K

  let one = inj M.one

  let mul_monomial p v =
    map (fun u -> inj (M.mul u v)) p

  let mul p q =
    map (fun v -> mul_monomial p v) q

  let leading leq (p:t) : K.t * M.t =
    let a = ref K.zero in
    let u = ref M.one in
    iter (fun b v -> if leq !u v then (a := b; u := v)) p;
    !a, !u
end
module FreeAlgebra (K : Field.T) (M : Monoid.T) = (Free(K)(M) : T)

(** Presentation of an algebra. *)
module Presentation (K : Field.T) (X : Alphabet.T) = struct
  module M = Monoid.Free(X)
  module A = Free(K)(M)

  type t =
    {
      leq : M.t -> M.t -> bool;
      generators : X.t list;
      rules : (M.t * A.t) list;
    }

  let free leq generators =
    let rules = [] in
    { leq; generators; rules }

  (** Orient a polynomial as a rule. *)
  let orient pres p =
    let a,u = A.leading pres.leq p in
    let p = A.cmul (K.inv a) p in
    let p = A.sub (A.inj u) p in
    u,p

  (** Add a rule to a presentation. *)
  let add_rule pres r =
    let rules = r :: pres.rules in
    { pres with rules }

  (** Add a relation to a presentation. *)
  let add_relation pres p =
    add_rule pres (orient pres p)

  (** Create a presentation from an alphabet and a list of monomials. *)
  let make leq generators pp : t =
    List.fold_left add_relation (free leq generators) pp

  (** Minimal reducible words. *)
  let heads pres =
    List.map (fun (u,p) -> u) pres.rules

  let to_string pres =
    "< "
    ^ String.concat " " (List.map X.to_string pres.generators) ^ " | "
    ^ String.concat " , " (List.map (fun (u,p) -> M.to_string u ^ " -> " ^ A.to_string p) pres.rules)
    ^ " >"

  (** Normalize words. *)
  let normalize pres p =
    let p = ref p in
    let loop = ref true in 
    while !loop do
      loop := false;
      p := A.map (fun u ->
        try
          let v,v' = List.find (fun (v,_) -> M.included v u) pres.rules in
          loop := true;
          let i = M.unifier u v in
          let v1 = M.sub u 0 i in
          let v2 = M.sub u (i + M.length v) (M.length u - (i + M.length v)) in
          let v1 = A.inj v1 in
          let v2 = A.inj v2 in
          A.mul v1 (A.mul v' v2)
        with
        | Not_found -> A.inj u
      ) !p
    done;
    !p

  (** Buchberger's completion algorithm. *)
  let buchberger pres =
    (* TODO: normalize the presentation first *)
    let todo = Queue.create () in
    List.iter (fun r -> Queue.add r todo) pres.rules;
    let pres = ref pres in
    (* Add a relation *)
    let rel p =
      (* Printf.printf "rel: %s\n%!" (A.to_string p); *)
      if not (A.eq A.zero p) then
        let r = orient !pres p in
        pres := add_rule !pres r;
        Queue.push r todo
    in
    while not (Queue.is_empty todo) do
      let u,p = Queue.pop todo in
      List.iter (fun (v,q) ->
        List.iter (fun ((u1,u2),(v1,v2)) ->
          (* Printf.printf "unifier: %s|%s->%s|%s vs %s|%s->%s|%s\n%!" *)
            (* (M.to_string u1) (M.to_string u) (A.to_string p) (M.to_string u2) *)
            (* (M.to_string v1) (M.to_string v) (A.to_string q) (M.to_string v2); *)
          (* Compute the S-polynomial *)
          let u1 = A.inj u1 in
          let u2 = A.inj u2 in
          let v1 = A.inj v1 in
          let v2 = A.inj v2 in
          let s1 = A.mul u1 (A.mul p u2) in
          let s2 = A.mul v1 (A.mul q v2) in
          let s = A.sub s1 s2 in
          let s = normalize !pres s in
          rel s
        ) (M.unifiers_bicontext u v)
      ) !pres.rules
    done;
    !pres

  (** Reduce a presentation. *)
  let reduce pres =
    let rules = pres.rules in
    let rules = List.map (fun (u,p) -> u, normalize pres p) rules in
    let rec aux acc = function
      | (u,p)::rules ->
         let f l = List.exists (fun (v,q) -> M.included v u) l in
         if f acc || f rules then aux acc rules else aux ((u,p)::acc) rules
      | [] -> List.rev acc
    in
    let rules = aux [] rules in
    { pres with rules }

  (** Algebra given by a convergent presentation. *)
  module Algebra (P : sig val presentation : t end) : T with type t = A.t = struct
    include A

    let nf = normalize P.presentation

    let inj m = nf (inj m)

    let cinj c m = nf (cinj c m)

    let mul p q = nf (mul p q)
  end

  (** Augmentations for presented algebras. *)
  module Augmentation = struct
    (** An augmentation. *)
    type t = A.t -> K.t

    (** Invalid augmentation. *)
    exception Invalid

    (** Construct an augmentation by defining it on generators. *)
    let make pres (eps : M.t -> K.t) : t =
      (* TODO: use a generic function *)
      let eps p =
        let ans = ref K.zero in
        A.iter (fun a u ->
          ans := K.add !ans (K.mul a (eps u))
        ) p;
        !ans
      in
      (* Ensure that it is well-defined. *)
      assert (K.eq K.one (eps (A.inj M.one)));
      List.iter (fun (u,p) ->
        if not (K.eq (eps (A.inj u)) (eps p)) then raise Invalid
      ) pres.rules;
      eps

    (** Traditional augmentation for graded algebras. *)
    let graded pres =
      make pres (fun u -> if M.eq M.one u then K.one else K.zero)

    (** Traditional augmentation for monoids / groups. *)
    let monoid pres =
      make pres (fun u -> K.one)
  end

  (** Anick resolution. *)
  module Anick = struct
    type chain = M.Anick.t

    module AMod = struct
      module Mod = Module.FreeRight(A)(M.Anick)

      include (Mod : module type of Mod with module Map := Mod.Map)

      (** Normalize a polynomial. *)
      let normalize pres p =
        let ans = ref zero in
        iter (fun u c ->
          let u = normalize pres u in
          ans := add !ans (cinj c u)
        ) p;
        !ans

      module Map = struct
        include Mod.Map

        (* We need to renormalize when applying a function. *)
        let bind pres f p =
          (* TODO: more efficient implementation? *)
          normalize pres (bind f p)
      end
    end

    (** Underlying [K]-module of [AMod]. *)
    module AKMod = struct
      type t = AMod.t
      type r = K.t
      let cinj a c u = AMod.cinj c (A.cinj a u)
      let inj c u = AMod.cmul c (A.inj u)
      let cmul a cu = AMod.cmul cu (A.cmul a A.one)
      let iter f (p:t) =
        AMod.iter (fun u c -> A.iter (fun a u -> f a c u) u) p
      (** Map a [K]-linear function. *)
      let map f p =
        let ans = ref AMod.zero in
        iter (fun a c u -> ans := AMod.add !ans (cmul a (f c u))) p;
        !ans
    end

    (** Array of Anick chains up to degree n. *)
    let chains pres n =
      let cc = Array.make (n+1) [M.Anick.empty] in
      if n >= 1 then cc.(1) <- M.Anick.singletons pres.generators;
      let left = List.map fst pres.rules in
      for i = 1 to n - 1 do
        cc.(i+1) <- M.Anick.extend left cc.(i)
      done;
      cc

    (** Compute the Anick resolution. *)
    let resolution ?augmentation pres n =
      let debug = false in
      (* TODO: check that the RS is convergent and reduced *)
      let augmentation = match augmentation with Some augmentation -> augmentation | None -> Augmentation.graded pres in
      if debug then Printf.printf "Resolving...\n%!";
      (* The Anick chains. *)
      let cc = chains pres n in
      (* The differential. *)
      let eps = augmentation in
      let eta a = A.cinj a M.one in
      let d = Array.init n (fun _ -> AMod.Map.zero) in
      List.iter (fun x ->
        d.(0) <- AMod.Map.set d.(0)
          (M.Anick.singleton x)
          (AMod.cinj
             (M.Anick.empty)
             (let x = A.inj (M.inj x) in
              A.sub x (eta (eps x)))
          );
        if debug then Printf.printf "d0(%s) = %s\n%!"
          (X.to_string x) (AMod.to_string (AMod.Map.app d.(0) (M.Anick.singleton x)))
      ) pres.generators;
      (* The contracting homotopy. *)
      let rec ch n p =
        if debug then Printf.printf "i%d(%s) = ?\n%!" n (AMod.to_string p);
        AMod.iter (fun u c -> assert (M.Anick.length c = n)) p;
        assert (n = 0 || AMod.eq AMod.zero (AMod.Map.bind pres d.(n-1) p));
        if AMod.eq AMod.zero p then AMod.zero
        else if n = 0 then
          (* Contract. *)
          let ans = ref AMod.zero in
          AKMod.iter (fun a c u ->
            if M.length u = 0 then
              (* assert (M.length u > 0) *)
              (* TODO: this is messy but should work (instead Anick defines the image of u-1) *)
              ()
            else
              for i = 0 to M.length u - 1 do
                let v = M.sub u 0 i in
                let c = M.Anick.singleton u.(i) in
                let u = M.sub u (i+1) (M.length u - (i+1)) in
                let a = K.mul a (eps (A.inj v)) in
                ans := AMod.add !ans (AKMod.cinj a c u)
              done
          ) p;
          !ans
        else
          (* Compute leading monomial. *)
          let a = ref K.zero in
          let c = ref M.Anick.empty in
          let u = ref M.one in
          let cu = ref M.one in
          (* TODO: remove this when the algorithm is working *)
          if not (AMod.eq AMod.zero (AMod.Map.bind pres d.(n-1) p)) then
            (* We are only defined on the kernel. *)
            failwith (AMod.to_string p ^ " not in the kernel.\n%!");
          AKMod.iter (fun a' c' u' ->
            let cu' = M.mul (M.Anick.eval c') u' in
            if pres.leq !cu cu' then
              (
                assert (not (K.eq a' K.zero));
                a := a';
                c := c';
                u := u';
                cu := cu'
              )
          ) p;
          let a = !a in
          let c = !c in
          let u = !u in
          (* Find where cn u is leftmost reducible. *)
          let cn = M.Anick.hd c in
          let cnl = M.length cn in
          let cnu = M.mul cn u in
          let cnul = M.length cnu in
          let i =
            let ans = ref (-1) in
            try
              for i = 0 to cnul - 1 do
                List.iter (fun (v,_) ->
                  let vl = M.length v in
                  if i + vl <= cnul && M.peq cnu i v 0 vl then
                    (
                      ans := i + vl - cnl;
                      raise Exit
                    )
                ) pres.rules
              done;
              Printf.printf "failed (non-reducible): %s\n%!" (AMod.to_string p);
              let leading = AKMod.cinj a c u in
              Printf.printf "leading: %s\n%!" (AMod.to_string leading);
              assert false
            with
            | Exit -> !ans
          in
          let u' = M.sub u 0 i in
          let u'' = M.sub u i (M.length u - i) in
          let c' = u' :: c in
          let ans = AKMod.cinj a c' u'' in
          let ans = AMod.normalize pres ans in
          (* Compute the remainder. *)
          let ans' = AMod.Map.bind pres d.(n) ans in
          let p' = AMod.sub p ans' in
          let p' = AMod.normalize pres p' in
          let ans' = ch n p' in
          let ans = AMod.normalize pres (AMod.add ans ans') in
          if debug then Printf.printf "i%d(%s) = %s\n%!" n (AMod.to_string p) (AMod.to_string ans);
          ans
      in
      (* Fill in higher differentials. *)
      for i = 1 to n - 1 do
        List.iter (fun c ->
          if debug then Printf.printf "\ndiff%d(%s) = ?\n%!" i (M.Anick.to_string c);
          let u = M.Anick.hd c in
          let c' = M.Anick.tl c in
          let p = AMod.cmul (AMod.inj c') (A.inj u) in
          (* Printf.printf "p: %s\n%!" (AMod.to_string p); *)
          let p' = AMod.Map.bind pres d.(i-1) p in
          (* Printf.printf "p': %s\n%!" (AMod.to_string p'); *)
          let p' = ch (i-1) p' in
          let p = AMod.sub p p' in
          let p = AMod.normalize pres p in
          if debug then Printf.printf "d%d(%s) = %s\n" i (M.Anick.to_string c) (AMod.to_string p);
          d.(i) <- AMod.Map.set d.(i) c p
        ) cc.(i+1)
      done;
      let cc = Array.map (fun l -> AMod.Presentation.make (Array.of_list l)) cc in
      let d = Array.mapi (fun i d -> AMod.Presentation.Map.of_map d cc.(i+1) cc.(i)) d in
      AMod.Presentation.Complex.make cc d

    module KMod = Module.Free(K)(M.Anick)
    module MF = Matrix.Functor(A)(K)
    module MFL = MF.Labeled(M.Anick)(M.Anick)

    (** Tor complex, whose homology is the one of the algebra (in right A-modules). *)
    let complex ?augmentation pres n =
      let augmentation = match augmentation with Some augmentation -> augmentation | None -> Augmentation.graded pres in
      let r = resolution ~augmentation pres n in
      let cc = AMod.Presentation.Complex.modules r in
      let d = AMod.Presentation.Complex.maps r in
      (* Tensor morphisms by the algebra. *)
      let d =
        let id x = x in
        Array.map (fun d ->
          MFL.map id id (fun p ->
            (* let x = ref K.zero in *)
            (* let p = normalize pres p in *)
            (* A.iter (fun y u -> x := K.add !x y) p; *)
            (* !x *)
            let p = normalize pres p in
            augmentation p
          ) d
        ) d
      in
      KMod.Presentation.Complex.make cc d

    let homology ?augmentation pres n =
      let c = complex ?augmentation pres (n+1) in
      KMod.Presentation.Complex.homology c
  end
end

module Generate (K : Field.T) (X : Alphabet.T with type t = int) = struct
  module Presentation = Presentation(K)(X)
  open Presentation
  module M = Presentation.M
  module A = Presentation.A

  let intset n =
    let rec aux k =
      if k >= n then [] else
        k::(aux (k+1))
    in
    aux 0

  let braid leq n =
    let generators = intset n in
    let relations = ref [] in
    for i = 0 to n - 2 do
     relations := (A.sub (A.inj [|i;i+1;i|]) (A.inj [|i+1;i;i+1|])) :: !relations
    done;
    for i = 0 to n - 1 do
      for j = i + 2 to n - 1 do
        relations := (A.sub (A.inj [|i;j|]) (A.inj [|j;i|])) :: !relations
      done
    done;
    let relations = List.rev !relations in
    make leq generators relations

  (** The symmetric algebra. *)
  let symmetric leq n =
    let generators = intset n in
    let relations = ref [] in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        relations := (A.sub (A.inj [|i;j|]) (A.inj [|j;i|])) :: !relations
      done
    done;
    let relations = List.rev !relations in
    make leq generators relations

  let exterior leq n =
    let generators = intset n in
    let relations = ref [] in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        relations := (A.sub (A.inj [|i;j|]) (A.cinj (K.neg K.one) [|j;i|])) :: !relations
      done
    done;
    for i = 0 to n - 1 do
      relations := (A.inj [|i;i|]) :: !relations
    done;
    let relations = List.rev !relations in
    make leq generators relations
end
