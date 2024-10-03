open Alg
open Term

let m = Op.make "m" 2
let ops = [m]

let () =
  List.iter
    (fun t ->
       print_endline (Term.to_string t)
    ) (Term.generate_ops ~at_most:true ops 3)
