let id x = x

let (|>) x f = f x

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
    aux n id l
end

module String = struct
  include String

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
