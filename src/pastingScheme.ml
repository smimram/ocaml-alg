(** Pasting schemes. *)

(** A pasting scheme. *)
type t = PS of t list

let to_list (PS l) = l

(** The point. *)
let point = PS []

(** Dimension. *)
let rec dim (PS l) = 1 + List.fold_left (fun d p -> max d (dim p)) (-1) l
let _dim = dim

let width (PS l) = List.length l

let eq = (=)

let compare = compare

(** String representation. *)
let rec to_string (PS l) = "[" ^ (String.concat "," @@ List.map to_string l) ^ "]"

(** Suspension. *)
let susp p = PS [p]

let bnd ?dim p =
  let dim = match dim with Some dim -> dim | None -> _dim p - 1 in
  assert (dim >= 0);
  let rec aux d (PS l) =
    if d = 0 then point
    else PS (List.map (aux (d-1)) l)
  in
  aux dim p

(** Source of a pasting scheme. *)
let src = bnd

(** Target of a pasting scheme. *)
let tgt = bnd
