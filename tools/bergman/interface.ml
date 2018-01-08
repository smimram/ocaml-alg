module Html = Dom_html

let doc = Html.document
let button txt action =
  let button_type = Js.string "button" in
  let b = Html.createInput ~_type:button_type doc in
  b##value <- Js.string txt;
  b##onclick <- Dom_html.handler (fun _ -> action (); Js._true);
  b

let debug s =
  Firebug.console##debug (Js.string s)

let jsget x = Js.Opt.get x (fun () -> assert false)

exception Parsing
  
let parse_pol =
  let lexer = Genlex.make_lexer ["+";"-";"*";"^";"(";")"] in
  let rec mults = function
    | [] -> `One
    | [x] -> x
    | x::l -> `Mul (x, mults l)
  in
  fun s ->
    let int n =
      if n = 0 then `Zero
      else if n = 1 then `One
      else if n = -1 then `Neg `One
      else raise Parsing
    in
    let rec add = parser
      | [< p = mul ; q = adds >] -> q p
    and adds = parser
        | [< 'Genlex.Kwd "+"; q = mul; r = adds >] -> fun p -> r (`Add (p,q))
        | [< 'Genlex.Kwd "-"; q = mul; r = adds >] -> fun p -> r (`Sub (p,q))
        | [< 'Genlex.Int n ; r = adds >] -> fun p ->
          if n >= 0 then raise Parsing
          else r (`Sub (p,int (-n)))
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
        | [< 'Genlex.Int n >] -> int n
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

let char_of_string s =
  assert (String.length s = 1);
  s.[0]

module M = struct
  include Monoid.Free(Alphabet.Char)

  let s s : t =
    Array.init (String.length s) (fun i -> s.[i])
end
module K = Field.Int
module P = Algebra.Presentation(K)(Alphabet.Char)
module A = P.A

let eval_pol p =
  let rec aux = function
    | `Mul (p,q) -> A.mul (aux p) (aux q)
    | `One -> A.one
    | `Zero -> A.zero
    | `Gen c -> A.inj (M.inj c)
    | `Add (p,q) -> A.add (aux p) (aux q)
    | `Sub (p,q) -> A.sub (aux p) (aux q)
    | `Neg p -> A.neg (aux p)
  in
  aux p

(** Replace [c] by [t] in [s]. *)
let rec replace c t s =
  try
    let n = String.index s c in
    replace c t (String.sub s 0 n ^ t ^ String.sub s (n+1) (String.length s-(n+1)))
  with
  | Not_found -> s

let run _ =
  let vars = jsget (Html.CoerceTo.textarea (jsget (doc##getElementById(Js.string "vars")))) in
  let relations = jsget (Html.CoerceTo.textarea (jsget (doc##getElementById(Js.string "relations")))) in
  let order = jsget (Html.CoerceTo.select (jsget (doc##getElementById(Js.string "order")))) in
  let generate = jsget (Html.CoerceTo.select (jsget (doc##getElementById(Js.string "generate")))) in
  let generaten = jsget (Html.CoerceTo.input (jsget (doc##getElementById(Js.string "generaten")))) in
  let augmentation = jsget (Html.CoerceTo.select (jsget (doc##getElementById(Js.string "augmentation")))) in
  let maxdeg = jsget (Html.CoerceTo.input (jsget (doc##getElementById(Js.string "maxdeg")))) in
  let grobner = jsget (doc##getElementById(Js.string "grobner")) in
  let chains = jsget (doc##getElementById(Js.string "chains")) in
  let resolution = jsget (doc##getElementById(Js.string "resolution")) in
  let betti = jsget (doc##getElementById(Js.string "betti")) in
  let go = jsget (doc##getElementById(Js.string "go")) in
  let status = jsget (doc##getElementById(Js.string "status")) in
  let status s = status##innerHTML <-Js.string s in
  let error s = status ("<em style=\"color:red\">" ^ s ^ "</em>") in
  
  go##onclick <- Html.handler (fun _ ->
    try
      let vars = Js.to_string vars##value in
      let relations = Js.to_string relations##value in

      grobner##innerHTML <- Js.string "";
      chains##innerHTML <- Js.string "";
      resolution##innerHTML <- Js.string "";
      betti##innerHTML <- Js.string "";

      status "Parsing variables...";
      let vars = String.split_on_char ',' vars in
      let vars = List.map String.trim vars in
      let vars = List.map char_of_string vars in

      status "Parsing relations...";
      let relations = String.split_on_char ',' relations in
      let relations = List.map String.trim relations in
      let relations =
        try
          List.map parse_pol relations
        with
        | Parsing -> error "Parsing error!"; raise Exit
      in
      grobner##innerHTML <- Js.string ("Relations: " ^ String.concat " , " (List.map string_of_pol relations));
      let relations = List.map eval_pol relations in
      let order =
        match Js.to_string order##value with
        | "deglex" -> M.Order.deglex Alphabet.Char.leq
        | "revdeglex" -> M.Order.deglex Alphabet.Char.geq
        | _ -> assert false
      in
      let pres = P.make order vars relations in
      let augmentation =
        try
          match Js.to_string augmentation##value with
          | "algebra" -> P.Augmentation.graded pres
          | "monoid" -> P.Augmentation.monoid pres
          | _ -> assert false
        with
        | P.Augmentation.Invalid ->
           error "Invalid augmentation!";
          raise Exit
      in

      status "Computing Gröbner basis...";
      let pres = P.reduce (P.buchberger pres) in
      grobner##innerHTML <- Js.string (P.to_string pres);
      
      status "Computing Anick chains...";
      let heads = P.heads pres in
      let cc = M.Anick.singletons vars in
      let cc = ref cc in
      let s = ref "" in
      for i = 0 to 6 do
        s := !s ^ string_of_int i ^ " chains: " ^ String.concat " " (List.map M.Anick.to_string !cc) ^ "<br/>";
        chains##innerHTML <- Js.string !s;
        cc := M.Anick.extend heads !cc
      done;

      status "Computing resolution...";
      let maxdeg = int_of_string (Js.to_string maxdeg##value) in
      let d = P.Anick.resolution ~augmentation pres (maxdeg+1) in
      resolution##innerHTML <- Js.string (replace '\n' "<br/>" (P.Anick.AMod.Presentation.Complex.to_string d));

      status "Computing Betti numbers...";
      let s = ref "" in
      let h = P.Anick.betti ~augmentation pres maxdeg in
      Array.iteri (fun i n -> s := !s ^ "H" ^ string_of_int i ^ " = " ^ string_of_int n ^ "<br/>") h;
      betti##innerHTML <- Js.string !s;

      status "Done.";
      Js._true
    with
    | Exit ->
       Js._false
    | Failure s ->
       error ("Error: " ^ s);
      Js._false
    | Not_found ->
       error "Not_found...";
      Js._false
  );

  let generate_handler _ =
    let list_init n f =
      let rec aux k = if k >= n then [] else (f k)::(aux (k+1)) in
      aux 0
    in
    let set v r =
      vars##value <- Js.string v;
      relations##value <- Js.string r
    in
    let gen n = String.make 1 (char_of_int (n + int_of_char 'a')) in
    let gen' n = String.make 1 (char_of_int (n + int_of_char 'A')) in
    let sym n =
      let v = String.concat "," (list_init n gen) in
      let rel = ref [] in
      for i = 0 to n-1 do
        for j = i+1 to n-1 do
          rel := (gen i^gen j^"-"^gen j^gen i) :: !rel
        done
      done;
      let rel = String.concat "," (List.rev !rel) in
      set v rel
    in
    let ext n =
      let v = String.concat "," (list_init n gen) in
      let rel = ref [] in
      for i = 0 to n-1 do
        rel := (gen i^gen i) :: !rel;
        for j = (i+1) to n-1 do
          rel := (gen i^gen j^"+"^gen j^gen i) :: !rel
        done
      done;
      let rel = String.concat "," (List.rev !rel) in
      set v rel
    in
    let symg n =
      let v = String.concat "," (list_init n gen) in
      let rel = ref [] in
      for i = 0 to n-2 do
        rel := (gen i^gen i^"-1") :: !rel;
        rel := (gen i^gen (i+1)^gen i^"-"^gen (i+1)^gen i^gen (i+1)) :: !rel
      done;
      for i = 0 to n-1 do
        for j = i+2 to n-1 do
          rel := (gen i^gen j^"-"^gen j^gen i) :: !rel
        done
      done;
      let rel = String.concat "," (List.rev !rel) in
      set v rel
    in
    let sklyanin n =
      let v = String.concat "," (list_init n gen) in
      let rel = ref "" in
      for i = 0 to n-1 do
        rel := !rel ^ (if !rel = "" then "" else "+") ^ gen i ^ "^" ^ string_of_int n
      done;
      rel := !rel ^ "+";
      for i = 0 to n-1 do
        rel := !rel ^ gen i
      done;
      set v !rel
    in
    let pow n =
      let v = "x,y" in
      let rel = ref [] in
      for i = 0 to n-1 do
        let s = ref "x" in
        for j = 0 to i-1 do
          s := !s ^ "y"
        done;
        s := !s ^ "x";
        rel := !s :: !rel
      done;
      let rel = String.concat "," (List.rev !rel) in
      set v rel
    in
    let plactic n =
      let v = list_init n gen in
      let v = String.concat "," v in
      let rel = ref [] in
      for x = 0 to n-1 do
        for y = x+1 to n-1 do
          for z = y to n-1 do
            let x = gen x in
            let y = gen y in
            let z = gen z in
            rel := (y^z^x^"-"^y^x^z) :: !rel
          done
        done
      done;
      for x = 0 to n-1 do
        for y = x to n-1 do
          for z = y+1 to n-1 do
            let x = gen x in
            let y = gen y in
            let z = gen z in
            rel := (x^z^y^"-"^z^x^y) :: !rel
          done
        done
      done;
      let rel = String.concat "," (List.rev !rel) in
      set v rel
    in
    let n = int_of_string (Js.to_string generaten##value) in
    (
      match Js.to_string generate##value with
      | "sym" -> sym n
      | "ext" -> ext n
      | "symg" -> symg n
      | "sklyanin" -> sklyanin n
      | "pow" -> pow n
      | "plactic" -> plactic n
      | _ -> assert false
    );
    Js._true
  in

  generate##oninput <- Html.handler generate_handler;
  generaten##onchange <- Html.handler generate_handler;
  Js._true

let () =
  Html.window##onload <- Html.handler run
