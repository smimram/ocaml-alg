(** The quaternion group. *)

open Alg
module Q = Group.Quaternion

module List = struct
  include List

  let map_pairs f l1 l2 =
    List.flatten @@ List.map (fun x -> List.map (f x) l2) l1
end

let () =
  Q.elements
  |> List.map Q.to_string
  |> String.concat " "
  |> Printf.printf "- elements: %s\n%!";
  List.map_pairs (fun x y -> Printf.sprintf "%s*%s=%s" (Q.to_string x) (Q.to_string y) (Q.to_string (Q.mul x y))) Q.elements Q.elements
  |> String.concat "\n"
  |> Printf.printf "- products:\n%s\n%!";
  print_newline ()

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
    ) [Q.one;Q.i;Q.j;Q.k]
