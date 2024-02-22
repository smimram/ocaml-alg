open Js_of_ocaml
open Alg

module Array = struct
  include Array

  let index_opt x a =
    let ans = ref (-1) in
    for i = 0 to Array.length a - 1 do
      if x = a.(i) then ans := i
    done;
    if !ans = -1 then None else Some !ans
end
module String = struct
  include String

  let split_on_first_char c s =
    let i = String.index s c in
    String.sub s 0 i,
    String.sub s (i+1) (String.length s - (i+1))

  (** Replace [c] by [t] in [s]. *)
  let rec replace c t s =
    try
      let n = String.index s c in
      replace c t (String.sub s 0 n ^ t ^ String.sub s (n+1) (String.length s-(n+1)))
    with Not_found -> s
end

module Str = struct
  include Str

  let split_first r s =
    let i = Str.search_forward r s 0 in
    let l = String.length (Str.matched_string s) in
    String.sub s 0 i,
    String.sub s (i+l) (String.length s - (i+l))
end

module Html = Dom_html

let doc = Html.document
let button txt action =
  let button_type = Js.string "button" in
  let b = Html.createInput ~_type:button_type doc in
  b##.value := Js.string txt;
  b##.onclick := Dom_html.handler (fun _ -> action (); Js._true);
  b

let debug s = Firebug.console##debug (Js.string s)

let jsget x = Js.Opt.get x (fun () -> assert false)

let run _ =
  let generate = jsget (Html.CoerceTo.select (jsget (doc##getElementById(Js.string "generate")))) in
  let generaten = jsget (Html.CoerceTo.input (jsget (doc##getElementById(Js.string "generaten")))) in
  let syms = jsget (Html.CoerceTo.textarea (jsget (doc##getElementById(Js.string "symbols")))) in
  let rules = jsget (Html.CoerceTo.textarea (jsget (doc##getElementById(Js.string "rules")))) in
  let parsed_presentation = jsget (doc##getElementById(Js.string "presentation")) in
  let completion = jsget (doc##getElementById(Js.string "completion")) in
  let reduced = jsget (doc##getElementById(Js.string "reduced")) in
  let coherence = jsget (doc##getElementById(Js.string "coherence")) in
  let go = jsget (doc##getElementById(Js.string "go")) in
  let status = jsget (doc##getElementById(Js.string "status")) in
  let status s = status##.innerHTML := Js.string s in
  let error s = status ("<em style=\"color:red\">" ^ s ^ "</em>") in

  go##.onclick :=
    Html.handler
      (fun _ ->
         try
           status "Started computation...";
           let syms =
             Js.to_string syms##.value
             |> String.split_on_char ','
             |> List.map String.trim
             |> Array.of_list
           in
           let rules =
             Js.to_string rules##.value
             |> String.split_on_char ','
             |> List.map (String.split_on_char '\n')
             |> List.flatten
             |> List.map String.trim
             |> List.filter (fun s -> s <> "")
             |> List.map (Str.split_first (Str.regexp "\\(=\\|→\\)"))
             |> List.map (fun (u,v) -> String.trim u, String.trim v)
           in

           let module X = struct
             include Alphabet.Int
             let to_string n = syms.(n)
             let of_char c =
               match Array.index_opt (String.make 1 c) syms with
               | Some i -> i
               | None -> failwith ("Unknown letter "^String.make 1 c)
           end in
           let module P = Monoid.Pres(X) in
           let string_of_rs rs =
             P.to_string rs
             |> Str.global_replace (Str.regexp "->") "→"
             |> Str.global_replace (Str.regexp "<") "⟨"
             |> Str.global_replace (Str.regexp ">") "⟩"
           in
           let string_of_coh coh =
             List.map
               (fun (p,q) ->
                  "<li>" ^
                  P.Path.to_string p ^
                  "<br/>" ^
                  P.Path.to_string q ^
                  "</li>"
               ) coh
             |> String.concat "\n"
             |> (fun s -> "<ul>" ^ s ^ "</ul>")
           in

           status "Parsing rewriting system...";
           let rules =
             let word u = if u = "1" || u = "ε" then [||] else Array.init (String.length u) (fun i -> X.of_char u.[i]) in
             List.map (fun (u,v) -> "", word u, word v) rules
           in
           let rs = P.make (List.init (Array.length syms) Fun.id) rules in
           parsed_presentation##.innerHTML := Js.string (string_of_rs rs);
           status ("Parsed: " ^ P.to_string rs);

           status "Computing Knuth-Bendix completion...";
           let rs = P.complete ~namer:P.Rule.Namer.none (P.W.Order.deglex X.leq) rs in
           completion##.innerHTML := Js.string (string_of_rs rs);

           status "Reducing presentation...";
           let rs = P.reduce rs in
           reduced##.innerHTML := Js.string (string_of_rs rs);

           status "Computing coherence cells...";
           let coh = P.coherence rs in
           coherence##.innerHTML := Js.string (string_of_coh coh);

           status "Done.";
           Js._true
         with
         | Exit ->
           Js._false
         | Failure s ->
           error ("Error: " ^ s);
           Js._false
         | e ->
           error (Printexc.to_string e);
           Js._false
      );

  let generate_handler _ =
    let set v r =
      let v = String.concat "," v in
      let r = List.map (fun (u,v) -> u^"="^v) r |> String.concat "," in
      syms##.value := Js.string v;
      rules##.value := Js.string r
    in
    let gen n = String.make 1 (char_of_int (n + int_of_char 'a')) in
    let dihedral n =
      let v = ["r";"s"] in
      let r =
        [
          String.make n 'r', "1";
          "ss", "1";
          "rs", "s" ^ String.make (n-1) 'r'
        ]
      in
      set v r
    in
    let quaternion n =
      let v = ["a";"b"] in
      let r =
        [
          String.make (1 lsl n) 'a',"1";
          "bbbb","1";
          String.make (1 lsl (n-1)) 'a',"bb";
          "aba","b"
        ]
      in
      set v r
    in
    let sym n =
      let v = List.init n gen in
      let rel = ref [] in
      for i = 0 to n-2 do
        rel := (gen i^gen (i+1)^gen i, gen (i+1)^gen i^gen (i+1)) :: !rel
      done;
      for i = 0 to n-1 do
        for j = i+2 to n-1 do
          rel := (gen i^gen j, gen j^gen i) :: !rel
        done
      done;
      for i = 0 to n-1 do
        rel := (gen i^gen i, "1") :: !rel
      done;
      set v !rel
    in
    let n = int_of_string (Js.to_string generaten##.value) in
    (
      match Js.to_string generate##.value with
      | "dihedral" -> dihedral n
      | "quaternion" -> quaternion n
      | "sym" -> sym n
      | _ -> assert false
    );
    Js._true
  in

  generate##.oninput := Html.handler generate_handler;
  generaten##.onchange := Html.handler generate_handler;
  Js._true

let () =
  Html.window##.onload := Html.handler run
