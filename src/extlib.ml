let pair x y = x, y

let unpair f (x,y) = f x y

module Int = struct
  include Int

  let modulo x y =
    let ans = x mod y in
    if ans >= 0 then ans
    else ans + y

  (** Split n in k parts (whose sum is n). *)
  let rec splits n k : int list list =
    if (n < 0 || k < 0) || (k = 0 && n > 0) then []
    else if k = 0 then [[]]
    else List.init (n+1) (fun i -> List.map (fun l -> i::l) (splits (n-i) (k-1))) |> List.flatten
end

module List = struct
  include List

  (* NOTE: backward compatibility with OCaml < 5.3 *)
  let rec take n l =
    if n = 0 then [] else
      match l with
      | x::l -> x::(take (n-1) l)
      | [] -> assert false

  (* NOTE: backward compatibility with OCaml < 5.3 *)
  let rec drop n l =
    if n = 0 then l else
      match l with
      | x::l -> drop (n-1) l
      | [] -> assert false

  (* NOTE: backward compatibility with OCaml < 5.1 *)
  let find_index p l =
    let rec aux n = function
      | x::l -> if p x then Some n else aux (n+1) l
      | [] -> None
    in
    aux 0 l

  (** Replace the nth element of a list. *)
  let replace_nth l n x =
    let rec aux n k = function
      | y::l ->
        if n = 0 then k (x::l)
        else aux (n-1) (fun l -> k (y::l)) l
      | [] -> raise Not_found
    in
    aux n Fun.id l

  let replace_assoc k v l =
    List.map (fun (k',v') -> if k = k' then k, v else k', v') l

  let sub l ofs len =
    take len @@ drop ofs l

  (** Product of a list of lists (returns the list of all combinations of elements). *)
  let rec products = function
    | l::ll -> List.map (fun x -> List.map (fun l -> x::l) (products ll)) l |> List.flatten
    | [] -> [[]]

  (** Map a function on pairs of elements of two lists. *)
  let map_pairs f l1 l2 =
    List.flatten @@ List.map (fun x -> List.map (fun y -> f x y) l2) l1
end

module String = struct
  include String

  let subscript s =
    String.fold_left
      (fun s -> function
         | '0' -> s ^ "₀"
         | '1' -> s ^ "₁"
         | '2' -> s ^ "₂"
         | '3' -> s ^ "₃"
         | '4' -> s ^ "₄"
         | '5' -> s ^ "₅"
         | '6' -> s ^ "₆"
         | '7' -> s ^ "₇"
         | '8' -> s ^ "₈"
         | '9' -> s ^ "₉"
         | '-' -> s ^ "₋"
         | _ -> assert false
      ) "" s

  let superscript s =
    String.fold_left
      (fun s -> function
         | '0' -> s ^ "⁰"
         | '1' -> s ^ "¹"
         | '2' -> s ^ "²"
         | '3' -> s ^ "³"
         | '4' -> s ^ "⁴"
         | '5' -> s ^ "⁵"
         | '6' -> s ^ "⁶"
         | '7' -> s ^ "⁷"
         | '8' -> s ^ "⁸"
         | '9' -> s ^ "⁹"
         | '-' -> s ^ "⁻"
         | _ -> assert false
      ) "" s

  (** Find the index of the first character matching a predicate. *)
  let find p s =
    let ans = ref (-1) in
    try
      for i = 0 to String.length s - 1 do
        if p s.[i] then
          (
            ans := i;
            raise Exit
          )
      done;
      raise Not_found
    with
    | Exit -> !ans

  let rec split_on_predicate p s =
    try
      let n = find p s in
      (String.sub s 0 n)::(split_on_predicate p (String.sub s (n+1) (String.length s - (n+1))))
    with Not_found -> [s]

  let find_index_from p s i =
    let ans = ref (-1) in
    try
      for i = i to String.length s - 1 do
        if p s.[i] then (
          ans := i;
          raise Exit
        )
      done;
      raise Not_found
    with Exit -> !ans

  (** Find the matching closing parenthesis of an opening parenthesis. *)
  let matching_parenthesis s i =
    let find_closing a b =
      let n = ref 0 in
      find_index_from
        (fun c ->
           if c = a then incr n
           else if c = b then decr n;
           assert (!n >= 0);
           !n = 0
        ) s i
    in
    let find_opening a b =
      let n = ref 0 in
      let ans = ref (-1) in
      try
        for i = i downto 0 do
          if s.[i] = b then incr n
          else if s.[i] = a then decr n;
          assert (!n >= 0);
          if !n = 0 then (
            ans := i;
            raise Exit
          )
        done;
        raise Not_found
      with Exit -> !ans
    in
    match s.[i] with
    | '(' -> find_closing '(' ')'
    | ')' -> find_opening '(' ')'
    | _ -> assert false
end

(** Sets of integers below some given bound, represented as arrays of words so that the bound can be arbitrary (and, contrarily to sets of the standard library, equal sets have equal representations). *)
module Bitset = struct
  (** Number of elements stored in each word. *)
  let bits = Sys.word_size - 2

  (** The empty set of elements below n. *)
  let empty n = Array.make (max 1 ((n + bits - 1) / bits)) 0

  (** The set with one element. *)
  let singleton n i =
    let s = empty n in
    s.(i / bits) <- 1 lsl (i mod bits);
    s

  (** The set of all the elements below n. *)
  let full n =
    let s = empty n in
    for i = 0 to n - 1 do
      s.(i / bits) <- s.(i / bits) lor (1 lsl (i mod bits))
    done;
    s

  (** Whether an element belongs to the bitset. *)
  let mem s i = s.(i / bits) land (1 lsl (i mod bits)) <> 0

  (** Union of bitsets. *)
  let union = Array.map2 (lor)

  (** Intersection of bitsets. *)
  let diff = Array.map2 (fun a b -> a land lnot b)

  (** Whether a bitset is empty. *)
  let is_empty = Array.for_all (fun a -> a = 0)

  (** Smallest element of a non-empty set. *)
  let min_elt s =
    let rec word i = if s.(i) <> 0 then i else word (i+1) in
    let i = word 0 in
    let rec bit j = if s.(i) land (1 lsl j) <> 0 then j else bit (j+1) in
    i * bits + bit 0
end
