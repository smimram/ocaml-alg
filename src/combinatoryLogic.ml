(** Combinatory logic. *)

open Extlib

(** A combinator. *)
type t = I | K | S | App of t * t

(** String representation. *)
let rec to_string = function
  | I -> "I"
  | K -> "K"
  | S -> "S"
  | App (t , u) ->
    let pa = match u with App _ -> true | _ -> false in
    let pa s = if pa then "("^s^")" else s in
    to_string t ^ " " ^ pa (to_string u)

(** Parser. *)
let rec of_string s =
  let s = String.trim s in
  let l = String.length s in
  match s with
  | "I" -> I
  | "K" -> K
  | "S" -> S
  | _ when s.[l-1] = ')' ->
    let i = String.matching_parenthesis s (l-1) in
    if i = 0 then of_string (String.sub s 1 (l-2))
    else
      let t = String.sub s 0 i |> of_string in
      let u = String.sub s i (l-i) |> of_string in
      App (t, u)
  | _ ->
    let t = String.sub s 0 (l-1) |> of_string in
    let u = String.sub s (l-1) 1 |> of_string in 
    App (t, u)

(** Normalize combinator. *)
(*
let rec normalize t =
  match t with
  | I | K | S -> t
  | App (t, v) ->
    match normalize t with
    | I -> normalize v
    | App (K, t) -> normalize t
    | App (App (S, t), u) -> normalize (App (App (t, v), App (u, v)))
    | t -> App (t, normalize v)
*)

let rec normalize t =
  let rec aux env = function
    | I ->
      (
        match env with
        | t::env -> aux env t
        | [] -> I
      )
    | K ->
      (
        match env with
        | t::u::env -> aux env t
        | [t] -> App (K, normalize t)
        | [] -> K
      )
    | S ->
      (
        match env with
        | t::u::v::env -> aux env (App (App (t,v), App (u,v)))
        | [t;u] -> App (App (S, normalize t), normalize u)
        | [t] -> App (S, normalize t)
        | [] -> S
      )
    | App (t, u) -> aux (u::env) t
  in
  aux [] t
