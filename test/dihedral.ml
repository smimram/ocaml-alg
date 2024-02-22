open Alg

module X = struct
  include Alphabet.Int
  let to_string = function
    | 0 -> "r"
    | 1 -> "s"
    | _ -> assert false
end
module P = Monoid.Pres(X)
module W = P.W
module G = Monoid.Generate(X)

let () =
  let presentation = G.dihedral in
  let pres = presentation 2 in
  print_endline ("presentation: " ^ P.to_string pres);
  let pres = P.complete (P.W.Order.deglex X.leq) pres in
  print_endline ("completed: " ^ P.to_string pres);
  let pres = P.reduce pres in
  print_endline ("reduced: " ^ P.to_string pres);
  print_endline ("branchings: " ^ (P.critical_branchings pres |> List.length |> string_of_int));
  List.iter
    (fun (p,q) ->
       print_endline (" - " ^ P.Path.to_string p);
       print_endline ("   " ^ P.Path.to_string q)
    ) (P.coherence pres);
  print_newline ();
  for i = 1 to 10 do
    presentation i
    |> P.complete (P.W.Order.deglex X.leq)
    |> P.reduce
    |> P.to_string
    |> Printf.printf "D%02d: %s\n%!" i
  done
