(** Combinatory logic. *)

(** A combinator. *)
type t = I | K | S | App of t * t

let rec to_string = function
  | I -> "I"
  | K -> "K"
  | S -> "S"
  | App (t , u) ->
    let pa = match u with App _ -> true | _ -> false in
    let pa s = if pa then "("^s^")" else s in
    to_string t ^ " " ^ pa (to_string u)

(** Normalize combinator. *)
let rec normalize t =
  match t with
  | I | K | S -> t
  | App (t, v) ->
    match normalize t with
    | I -> v
    | App (K, t) -> normalize t
    | App (App (S, t), u) -> normalize (App (App (t, v), App (u, v)))
    | t -> App (t, normalize v)
