(** Testing counting functions. *)

open Alg

(* Higher Bruhat orders. *)
let () =
  let b = Count.higher_bruhat_orders in
  print_string "# Number of elements of the higher Bruhat orders B(n,k)\n\n";
  for n = 1 to 7 do
    Printf.printf "n = %d:" n;
    for k = 1 to n do
      Printf.printf " %d" (b n k)
    done;
    print_newline ()
  done;
  let rec fact n = if n = 0 then 1 else n * fact (n-1) in
  (* The weak order on permutations. *)
  for n = 1 to 7 do
    assert (b n 1 = fact n)
  done;
  (* Rhombic tilings of 2n-gons (OEIS A006245). *)
  List.iteri (fun i c -> assert (b (i+2) 2 = c)) [1; 2; 8; 62; 908; 24698; 1232944];
  (* The formulas given by Ziegler in the case where k is close to n. *)
  for n = 1 to 8 do
    assert (b n n = 1);
    if n >= 2 then assert (b n (n-1) = 2);
    if n >= 3 then assert (b n (n-2) = 2 * n);
    if n >= 4 then assert (b n (n-3) = (1 lsl n) + n * (1 lsl (n-2)) - 2 * n)
  done;
  (* Compare with the definition, by enumerating all the sets of (k+1)-element subsets of [n] and keeping the consistent ones: for each (k+2)-element subset, the intersection with its packet, sorted in lexicographic order, should be a prefix or a suffix. *)
  let brute n k =
    let rec subsets m r =
      if r = 0 then [[]]
      else if r > m then []
      else subsets (m-1) r @ List.map (fun x -> x @ [m]) (subsets (m-1) (r-1))
    in
    let elements = subsets n (k+1) in
    let position x =
      let rec aux i = function
        | y::l -> if x = y then i else aux (i+1) l
        | [] -> assert false
      in
      aux 0 elements
    in
    let rec is_prefix = function
      | false::l -> List.for_all not l
      | true::l -> is_prefix l
      | [] -> true
    in
    let is_prefix_or_suffix l = is_prefix l || is_prefix (List.rev l) in
    (* The packet of X in lexicographic order is obtained by removing the last element first. *)
    let packets =
      List.map
        (fun x ->
           List.rev (List.mapi (fun i _ -> position (List.filteri (fun j _ -> j <> i) x)) x)
        ) (subsets n (k+2))
    in
    let count = ref 0 in
    for i = 0 to (1 lsl List.length elements) - 1 do
      if List.for_all (fun p -> is_prefix_or_suffix (List.map (fun j -> i land (1 lsl j) <> 0) p)) packets then incr count
    done;
    !count
  in
  List.iter
    (fun (n,k) -> assert (b n k = brute n k))
    [2,1; 3,1; 3,2; 4,1; 4,2; 4,3; 5,2; 5,3; 5,4; 6,3; 6,4; 6,5];
  (* A value which is not covered by the above checks. *)
  assert (b 7 3 = 7686);
  (* Invalid arguments. *)
  List.iter
    (fun (n,k) ->
       assert (match b n k with _ -> false | exception Invalid_argument _ -> true)
    ) [3,0; 3,4; 0,0; 3,-1]
