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

  let rec nth n = function
    | x::l -> if n = 0 then x else nth (n-1) l
    | [] -> raise Not_found
end
