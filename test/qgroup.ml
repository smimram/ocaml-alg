(** The quaternion group. *)

open Alg
module List = struct
  include List

  let map_pairs f l1 l2 =
    List.flatten @@ List.map (fun x -> List.map (f x) l2) l1
end

let () =
  let module Q = Group.Quaternion in
  Q.elements
  |> List.map Q.to_string
  |> String.concat " "
  |> Printf.printf "- elements: %s\n%!";
  List.map_pairs (fun x y -> Printf.sprintf "%s*%s=%s" (Q.to_string x) (Q.to_string y) (Q.to_string (Q.mul x y))) Q.elements Q.elements
  |> String.concat "\n"
  |> Printf.printf "- products:\n%s\n%!";
  print_newline ()

module Q = struct
  include Group.Quaternion

  let to_string x =
    if is_negative x then to_string (neg x) ^ "^-" else to_string x
end

module Edge = struct
  type t = X | Y | Z | W

  let to_string = function
    | X -> "x"
    | Y -> "y"
    | Z -> "z"
    | W -> "w"

  let elements = [X;Y;Z;W]

  let phi = function
    | X -> Q.i
    | Y -> Q.j
    | Z -> Q.neg Q.k
    | W -> Q.one
end

let () =
  List.iter
    (fun n ->
       List.iter
         (fun e ->
            let n' = Q.neg n in
            Printf.printf {|%s%s&:a%s\to b%s&%s%s&:a%s\to b%s\\|}
              (Edge.to_string e)
              (Q.to_string n)
              (Q.to_string n)
              (Q.to_string (Q.mul n (Edge.phi e)))
              (Edge.to_string e)
              (Q.to_string n')
              (Q.to_string n')
              (Q.to_string (Q.mul n' (Edge.phi e)))
            ;
            print_newline ()
         ) Edge.elements
    ) [Q.one;Q.i;Q.j;Q.k];
  print_newline ()

(*
let () =
  List.iter
    (fun q ->
       Printf.printf {|\begin{tikzcd}[sep=small]
(b,%s)&\ar[l,"{(z,%s)}"']\ar[d,"{(w,%s)}"](a,%s)\\
(a,%s)\ar[u,"{(y,%s)}"]\ar[r,"{(x,%s)}"']&(b,%s)
\end{tikzcd}
\qquad
|}
         (Q.to_string (Q.mul q Q.j))
         (Q.to_string (Q.mul q Q.i))
         (Q.to_string (Q.mul q Q.i))
         (Q.to_string (Q.mul q Q.i))
         (Q.to_string q)
         (Q.to_string q)
         (Q.to_string q)
         (Q.to_string (Q.mul q Q.i))
    ) Q.elements
*)

(*
let () =
  List.iter
    (fun q ->
       Printf.printf {|\begin{tikzcd}[sep=small]
(b,%s)&\ar[l,"{(w,%s)}"']\ar[d,"{(x,%s)}"](a,%s)\\
(a,%s)\ar[u,"{(y,%s)}"]\ar[r,"{(z,%s)}"']&(b,%s)
\end{tikzcd}
\qquad
|}
         (Q.to_string (Q.mul q Q.j))
         (Q.to_string (Q.mul q Q.j))
         (Q.to_string (Q.mul q Q.j))
         (Q.to_string (Q.mul (Q.mul q (Q.neg Q.k)) (Q.neg Q.i)))
         (Q.to_string q)
         (Q.to_string q)
         (Q.to_string q)
         (Q.to_string (Q.mul q (Q.neg Q.k)))
    ) Q.elements
*)

let () =
  List.iter
    (fun q ->
       Printf.printf {|\begin{tikzcd}[sep=small]
(b,%s)&\ar[l,"{(w,%s)}"']\ar[d,"{(y,%s)}"](a,%s)\\
(a,%s)\ar[u,"{(z,%s)}"]\ar[r,"{(x,%s)}"']&(b,%s)
\end{tikzcd}
\qquad
|}
         (Q.to_string (Q.mul q (Q.neg Q.k)))
         (Q.to_string (Q.mul q (Q.neg Q.k)))
         (Q.to_string (Q.mul q (Q.neg Q.k)))
         (Q.to_string (Q.mul q (Q.neg Q.k)))
         (Q.to_string q)
         (Q.to_string q)
         (Q.to_string q)
         (Q.to_string (Q.mul q Q.i))
    ) Q.elements
