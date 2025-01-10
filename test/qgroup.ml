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
