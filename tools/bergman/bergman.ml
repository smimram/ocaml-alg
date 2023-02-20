open Alg

module M = Monoid.Free(Alphabet.Char)
module K = Field.Int
module P = Algebra.Pres(K)(Alphabet.Char)
module A = P.A

let char_of_string s =
  assert (String.length s = 1);
  s.[0]

let parse_pol s =
  Parser.main Lexer.token (Lexing.from_string s)

let eval_pol p =
  let rec aux = function
    | Pol.Mul (p,q) -> A.mul (aux p) (aux q)
    | One -> A.one
    | Zero -> A.zero
    | Gen c -> A.inj (M.inj c)
    | Add (p,q) -> A.add (aux p) (aux q)
    | Sub (p,q) -> A.sub (aux p) (aux q)
    | Neg p -> A.neg (aux p)
  in
  aux p

let () =
  let order = ref "deglex" in
  let variables = ref "" in
  let relations = ref "" in
  Arg.parse [
    "--variables", Arg.Set_string variables, "Used variables."
  ] (fun s -> relations := s) "Usage: bergman [options]";
  let variables = !variables |> String.split_on_char ',' |> List.map (fun s -> s |> String.trim |> char_of_string) in
  let relations = !relations |> String.split_on_char ',' |> List.map (fun s -> s |> String.trim |> parse_pol |> eval_pol) in
  let order =
    match !order with
    | "deglex" -> M.Order.deglex Alphabet.Char.leq
    | "revdeglex" -> M.Order.deglex Alphabet.Char.geq
    | _ -> assert false
  in
  let pres = P.make order variables relations in
  Printf.printf "# Computing Gröbner basis\n\n%!";
  let pres = P.buchberger pres in
  let pres = P.reduce pres in
  Printf.printf "%s\n\n%!" (P.to_string pres)
