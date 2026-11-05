(** Pasting schemes. *)

(** A pasting scheme. *)
type t = PS of t list

let to_list (PS l) = l

(** The point. *)
let point = PS []

(** Dimension. *)
let rec dim (PS l) = 1 + List.fold_left (fun d p -> max d (dim p)) (-1) l

let width (PS l) = List.length l

let eq = (=)

let compare = compare

(** String representation. *)
let rec to_string (PS l) = "[" ^ (String.concat "," @@ List.map to_string l) ^ "]"
