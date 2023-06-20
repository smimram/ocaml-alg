open Js_of_ocaml
open Alg

module Array = struct
  include Array

  let index x a =
    let ans = ref 0 in
    for i = 0 to Array.length a - 1 do
      if x = a.(i) then ans := i
    done;
    !ans
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
  let syms = jsget (Html.CoerceTo.textarea (jsget (doc##getElementById(Js.string "symbols")))) in
  let rules = jsget (Html.CoerceTo.textarea (jsget (doc##getElementById(Js.string "rules")))) in
  let completion = jsget (doc##getElementById(Js.string "completion")) in
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
             |> List.map (String.split_on_first_char '=')
             |> List.map (fun (u,v) -> String.trim u, String.trim v)
           in

           let module X = struct
             include Alphabet.Int
             let to_string n = syms.(n)
             let of_char c = Array.index (String.make 1 c) syms
           end in
           let module P = Monoid.Pres(X) in

           status "Parsing rewriting system...";
           let rules =
             let word u = Array.init (String.length u) (fun i -> X.of_char u.[i]) in
             List.map (fun (u,v) -> word u, word v) rules
           in
           let rs = P.make (List.init (Array.length syms) Fun.id) rules in
           status ("Parsed: " ^ P.to_string rs);

           status "Computing Knuth-Bendix completion...";
           let display rs =
             completion##.innerHTML := Js.string (String.replace '\n' "<br/>" (P.to_string rs))
           in
           let rs = P.complete (P.W.Order.deglex X.leq) rs in
           display rs;

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

  Js._true

let () =
  Html.window##.onload := Html.handler run
