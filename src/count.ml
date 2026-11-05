(** Counting stuff. *)

open Extlib

(** The number of elements of the higher Bruhat order B(n,k) of Manin and Schechtman, for 1 ≤ k ≤ n. For instance, B(n,1) is the weak order on permutations, of cardinal n!, and the cardinals of B(n,2) are the numbers of rhombic tilings of a 2n-gon (OEIS A006245): 1, 2, 8, 62, 908, 24698, ...

    We do not know of a closed formula in general, and we proceed by enumeration, following H. Chau, {e On enumerating higher Bruhat orders through deletion and contraction}, 2024, whose notations we use. We only need the following definitions and results.

    The {e packet} of a set X of integers is the collection of subsets obtained by removing one element from X. We write X_i for the set obtained by removing the i-th element of X, so that the packet (X_(k+1), ..., X_1) of a (k+1)-element set X is sorted in lexicographic order. A set I of k-element subsets of [n] = {1, ..., n} is {e consistent} when, for every (k+1)-element subset X of [n], the intersection of I with the packet of X is either a prefix or a suffix of the packet. We write C(n,k) for the collection of consistent sets. A theorem of Ziegler states that B(n,k) is in bijection with C(n,k+1), so that we are left with counting consistent sets.

    Given a consistent set S in C(n,k), its {e consistent poset} P_S has the (k-1)-element subsets of [n] as elements, and its order is the transitive closure of the following relations: for every k-element subset X of [n] and 1 ≤ i < k,

    - X_i < X_(i+1) if X belongs to S,
    - X_(i+1) < X_i if X does not belong to S.

    In other words, a packet is sorted in one direction or the other, depending on whether X is in S. Chau's lemma 4.9 states that the consistent sets I of C(n+1,k) are in bijection with pairs (S,T), where S is in C(n,k) and T is an upper ideal of P_S (a subset containing all the elements above any of its elements). Namely, S consists of the elements of I which do not contain n+1 (this is called the deletion of n+1) and T consists of the elements of I which contain n+1, from which we remove n+1 (this is called the contraction of n+1). Thus we have

    |C(n+1,k)| = Σ_S (number of upper ideals of P_S)

    where S ranges over C(n,k). We compute this by induction on n, the case where n = k being immediate since there is only one k-element subset of [k], which can either be in the consistent set or not. Note that, even though T belongs to C(n,k-1) (this is lemma 4.8), we do not have to compute this collection, which means that we only use a single value of k during the induction.

    Geometrically, an element of B(n,k) can be thought of as an arrangement of n pseudo-hyperplanes, S describes the arrangement of the first n-1 ones and T describes where the last one goes.

    The running time is proportional to the number of elements of C(n-1,k+1), times the cost of counting ideals. In particular, this is not meant to compute large values.

    @raise Invalid_argument if we do not have 1 ≤ k ≤ n.
    @raise Failure if the result is too large to be represented. *)
let higher_bruhat_orders n k =
  if k < 1 || k > n then invalid_arg "Count.higher_bruhat_orders: we should have 1 ≤ k ≤ n";
  (* Addition which fails on overflow. *)
  let plus a b =
    let c = a + b in
    if c < a then failwith "Count.higher_bruhat_orders: overflow";
    c
  in
  (* Consistent sets of (k+1)-element subsets are in bijection with B(n,k): this is the only size of subsets we consider. *)
  let size = k + 1 in
  (* The r-element subsets of [m], as sorted lists, in colexicographic order: we first have the subsets which do not contain m and then those which do. The point of this order is that the subsets of [m] are a prefix of the subsets of [m+1]. Therefore the position of a subset in this list only depends on the subset and not on m, and we can represent a set of subsets by an array of booleans, extending a set from [m] to [m+1] simply amounting to adding elements at the end of the array. *)
  let rec subsets m r =
    if r = 0 then [[]]
    else if r > m then []
    else subsets (m-1) r @ List.map (fun x -> x @ [m]) (subsets (m-1) (r-1))
  in
  (* For every k+1-element subset X of [m], the positions, in the subsets of k elements, of X_1, X_2, ..., which are the elements of the packet of X. Those are the only information about subsets we need, and we compute them once for all. Since the removal of the i-th element yields lists in the expected order, those are in the order required by the definition of the consistent poset. *)
  let packets m =
    let position = Hashtbl.create 100 in
    List.iteri (fun i x -> Hashtbl.add position x i) (subsets m (size-1));
    subsets m size
    |> List.map
      (fun x ->
         List.mapi (fun i _ -> Hashtbl.find position (List.filteri (fun j _ -> j <> i) x)) x
         |> Array.of_list)
    |> Array.of_list
  in
  (* The packets in [m], for each m. We have that m is at least the size since otherwise there is nothing to count. *)
  let packets = Array.init (n+1) (fun m -> if m < size then [||] else packets m) in
  (* The number of subsets with k elements in [m], which is the number of elements of the consistent poset. *)
  let elements = Array.init (n+1) (fun m -> List.length (subsets m (size-1))) in
  (* The elements above and below each element in the consistent poset of the consistent set s in C(m,k+1). *)
  let poset m s =
    let e = elements.(m) in
    let up = Array.init e (Bitset.singleton e) in
    (* The generating relations of the poset, as pairs (a,b) meaning that a is below b. *)
    let relations = ref [] in
    Array.iteri
      (fun x packet ->
         for i = 0 to Array.length packet - 2 do
           let a, b = packet.(i), packet.(i+1) in
           relations := (if s.(x) then (a,b) else (b,a)) :: !relations
         done
      ) packets.(m);
    (* Compute the transitive closure: we iterate until nothing changes, which terminates since the union only grows (and the poset is known to be acyclic). *)
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter
        (fun (a,b) ->
           let u = Bitset.union up.(a) up.(b) in
           if u <> up.(a) then
             (
               up.(a) <- u;
               changed := true
             )
        ) !relations
    done;
    let down = Array.init e (fun _ -> Bitset.empty e) in
    for a = 0 to e - 1 do
      for b = 0 to e - 1 do
        if Bitset.mem up.(a) b then down.(b) <- Bitset.union down.(b) (Bitset.singleton e a)
      done
    done;
    up, down
  in
  (* Both of the functions below rely on the same remark in order to find the upper ideals among the elements which are not decided yet: if we pick an element i then either i is not in the ideal, in which case the elements below i are not in the ideal, or i is in the ideal, in which case the elements above i are in the ideal. In both cases, the remaining elements are not decided yet and the two cases are disjoint. *)
  (* The number of upper ideals of the consistent poset of s, where we memoize on the undecided elements (since the number of upper ideals only depends on those). *)
  let count_ideals m s =
    let up, down = poset m s in
    let memo = Hashtbl.create 100 in
    let rec aux rest =
      if Bitset.is_empty rest then 1 else
        match Hashtbl.find_opt memo rest with
        | Some c -> c
        | None ->
          let i = Bitset.min_elt rest in
          let c = plus (aux (Bitset.diff rest down.(i))) (aux (Bitset.diff rest up.(i))) in
          Hashtbl.add memo rest c;
          c
    in
    aux (Bitset.full elements.(m))
  in
  (* Iterate a function on all the upper ideals of the consistent poset of s. *)
  let iter_ideals m s f =
    let e = elements.(m) in
    let up, down = poset m s in
    let rec aux rest ideal =
      if Bitset.is_empty rest then f ideal else
        let i = Bitset.min_elt rest in
        aux (Bitset.diff rest down.(i)) ideal;
        aux (Bitset.diff rest up.(i)) (Bitset.union ideal up.(i))
    in
    aux (Bitset.full e) (Bitset.empty e)
  in
  (* The number of elements of C(n,k+1) whose restriction to [m] is the consistent set s of C(m,k+1). We explore the consistent sets in a depth-first way, by adding the integers m+1, ..., n one by one, which means that we do not need to store them. The last integer is handled by counting the ideals, without enumerating them: this is what makes the computation of the last step cheap, and it is by far the most numerous one. *)
  let rec count m s =
    if m = n then 1
    else if m = n - 1 then count_ideals m s
    else
      let total = ref 0 in
      let e = elements.(m) in
      iter_ideals m s
        (fun ideal ->
           (* The new consistent set: s together with the X ∪ {m+1} for X in the ideal. By the choice of the order on subsets, these come after the elements of s, in the same order as in the ideal. *)
           let s' = Array.append s (Array.init e (Bitset.mem ideal)) in
           total := plus !total (count (m+1) s')
        );
      !total
  in
  (* When k = n, there are no (k+1)-element subsets of [n] and the only consistent set is empty. Otherwise, we start from the two consistent sets of C(k+1,k+1). *)
  if k = n then 1
  else plus (count size [|false|]) (count size [|true|])
