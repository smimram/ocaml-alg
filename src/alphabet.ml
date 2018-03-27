(** Alphabets. *)

(** An alphabet is simply another name for a set, but the module [Set] already
    exists. *)

(** An alphabet. *)
module type T = sig
  (** A letter. *)
  type t

  val eq : t -> t -> bool

  val to_string : t -> string

  val compare : t -> t -> int
end

(** The alphabet of characters. *)
module Char = struct
  type t = char

  let eq c d = (c:char) = (d:char)

  let to_string c = String.make 1 c

  let compare c d = compare (c:char) (d:char)

  let leq c d = (c:char) <= (d:char)

  let geq c d = (c:char) >= (d:char)
end
module CharAlphabet : (T with type t = char) = Char

(* Backup since we need it afterward. *)
module Str = String

module String : (T with type t = string) = struct
  type t = string
  let eq s t = (s:string) = (t:string)

  let to_string s = s

  let compare s  t = compare (s:string) (t:string)
end

(** The alphabet of integers. *)
module Int = struct
  type t = int
  let eq i j = (i:int) = (j:int)

  let to_string = string_of_int

  let compare i j = compare (i:int) (j:int)

  let leq i j = (i:int) <= (j:int)

  let geq i j = (i:int) >= (j:int)
end
module IntAlphabet : (T with type t = int) = Int

(** The alphabet with one element. *)
module Unit : T with type t = unit = struct
  type t = unit

  let eq () () = true

  let compare () () = 0

  let to_string () = "*"
end

(** Product of alphabets. *)
module Prod (A:T) (B:T) : (T with type t = A.t * B.t) = struct
  type t = A.t * B.t

  let eq (a,b) (a',b') =
    A.eq a a' && B.eq b b'

  let compare (a,b) (a',b') =
    let c = A.compare a a' in
    if c = 0 then B.compare b b'
    else c

  let to_string (a,b) =
    "(" ^ A.to_string a ^ "," ^ B.to_string b ^ ")"
end

(** Triple product of alphabets. *)
module Prod3 (A:T) (B:T) (C:T) : (T with type t = A.t * B.t * C.t) = struct
  type t = A.t * B.t * C.t

  let eq (a,b,c) (a',b',c') =
    A.eq a a' && B.eq b b' && C.eq c c'

  let compare (a,b,c) (a',b',c') =
    let comp = A.compare a a' in
    if comp <> 0 then comp else
      let comp = B.compare b b' in
      if comp <> 0 then comp else
        C.compare c c'

  let to_string (a,b,c) =
    "(" ^ A.to_string a ^ "," ^ B.to_string b ^ "," ^ C.to_string c ^ ")"
end

(** Powerset. *)
module Pow (A:T) = struct
  module S = Set.Make(A)
  type t = S.t
  let eq (u:t) (v:t) = S.equal u v
  let compare (u:t) (v:t) = S.compare u v
  let to_string (u:t) =
    let s = S.fold (fun x s -> if s = "" then A.to_string x else s ^ "," ^ A.to_string x) u "" in
    "{" ^ s ^ "}"
  let empty : t = S.empty
  let of_list l : t = S.of_list l
  let add (u:t) (x:A.t) = S.add x u
  let mem (u:t) (x:A.t) = S.mem x u
  let iter (f:A.t->unit) (u:t) = S.iter f u
end
module PowAlphabet (A:T) : T = Pow(A)

(** Free monoid monad. *)
module List (A:T) = struct
  type t = A.t list

  let eq u v =
    try
      List.for_all2 (fun x y -> A.eq x y) u v
    with
    | Invalid_argument _ -> false

  let to_string u =
    Str.concat "" (List.map A.to_string u)

  let rec compare u v =
    match u,v with
    | x::u, y::v ->
       let c = A.compare x y in
       if c <> 0 then c else compare u v
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
end
module ListAlphabet (A:T) : T = List(A)

(** Functions between alphabets. *)
module Map (A:T) (B:T) = struct
  module M = Map.Make(A)

  type t = B.t M.t

  let empty : t = M.empty

  let app (f:t) (x:A.t) = M.find x f

  let add (f:t) (x:A.t) v : t = M.add x v f

  (** Is an element in the domain? *)
  let mem (f:t) (x:A.t) = M.mem x f

  let iter = M.iter
end
