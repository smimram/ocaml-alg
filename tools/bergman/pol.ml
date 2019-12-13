(** Polynomial expressions. *)
type t =
  | Mul of t * t
  | One
  | Zero
  | Gen of char
  | Add of t * t
  | Sub of t * t
  | Neg of t

let to_string p =
  let rec aux = function
    | Mul (p,q) -> aux p ^ "*" ^ aux q
    | One -> "1"
    | Zero -> "0"
    | Gen c -> String.make 1 c
    | Add (p,q) -> aux p ^ "+" ^ aux q
    | Sub (p,q) -> aux p ^ "-" ^ aux q
    | Neg p -> "-" ^ aux p
  in
  aux p


