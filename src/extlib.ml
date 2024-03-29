let pair x y = x, y

let unpair f (x,y) = f x y

module Int = struct
  let modulo x y =
    let ans = x mod y in
    if ans >= 0 then ans
    else ans + y
end

module List = struct
  include List

  (** First index where a predicate is satisfied. *)
  let index p l =
    let rec aux n = function
      | x::l -> if p x then n else aux (n+1) l
      | [] -> raise Not_found
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

  let rec sub l ofs len =
    if ofs = 0 && len = 0 then []
    else
      match l with
      | x::l ->
        if ofs = 0 then x::(sub l ofs (len-1))
        else sub l (ofs-1) len
      | [] -> invalid_arg "List.sub"
end

module String = struct
  include String

  let subscript s =
    String.fold_left
      (fun s -> function
         | '0' -> "₀"
         | '1' -> "₁"
         | '2' -> "₂"
         | '3' -> "₃"
         | '4' -> "₄"
         | '5' -> "₅"
         | '6' -> "₆"
         | '7' -> "₇"
         | '8' -> "₈"
         | '9' -> "₉"
         | _ -> assert false
      ) "" s

  let superscript s =
    String.fold_left
      (fun s -> function
         | '0' -> "⁰"
         | '1' -> "¹"
         | '2' -> "²"
         | '3' -> "³"
         | '4' -> "⁴"
         | '5' -> "⁵"
         | '6' -> "⁶"
         | '7' -> "⁷"
         | '8' -> "⁸"
         | '9' -> "⁹"
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
end
