let parse_pol =
  let lexer = Genlex.make_lexer ["+";"-";"*";"^";"(";")"] in
  let rec mults = function
    | [] -> `One
    | [x] -> x
    | x::l -> `Mul (x, mults l)
  in
  fun s ->
    let rec add = parser
      | [< p = mul ; q = adds >] -> q p
    and adds = parser
        | [< 'Genlex.Kwd "+"; q = mul; r = adds >] -> fun p -> r (`Add (p,q))
        | [< 'Genlex.Kwd "-"; q = mul; r = adds >] -> fun p -> r (`Sub (p,q))
        | [< >] -> fun p -> p
    and mul = parser
        | [< p = exp ; q = muls >] -> q p
        | [< p = exp >] -> p
    and muls = parser
        | [< 'Genlex.Kwd "*"; q = exp; r = muls >] -> fun p -> r (`Mul (p,q))
        | [< >] -> fun p -> p
    and exp = parser
        | [< p = atom; q = exps >] -> q p
    and exps = parser
        | [< 'Genlex.Kwd "^" ; 'Genlex.Int n >] ->
           fun p ->
             let rec aux = function
               | 0 -> `One
               | 1 -> p
               | n -> `Mul (p, aux (n-1))
             in
             aux n
        | [< >] -> fun p -> p
    and atom = parser
        | [< 'Genlex.Char c >] ->
           `Gen c
        | [< 'Genlex.Ident s >] ->
           let ans = ref [] in
           for i = String.length s - 1 downto 0 do
             ans := (`Gen s.[i]) :: !ans
           done;
           mults !ans
        | [< 'Genlex.Int n >] -> if n=0 then `Zero else if n=1 then `One else assert false
        | [< 'Genlex.Kwd "-"; p = atom >] -> `Neg p
        | [< 'Genlex.Kwd "("; p = add; 'Genlex.Kwd ")" >] -> p
    in
    add (lexer (Stream.of_string s))

let string_of_pol p =
  let rec aux = function
    | `Mul (p,q) -> aux p ^ "*" ^ aux q
    | `One -> "1"
    | `Zero -> "0"
    | `Gen c -> String.make 1 c
    | `Add (p,q) -> aux p ^ "+" ^ aux q
    | `Sub (p,q) -> aux p ^ "-" ^ aux q
    | `Neg p -> "-" ^ aux p
  in
  aux p

let () =
  let p = parse_pol "xx-yy" in
  print_string (string_of_pol p ^ "\n")
