open Js_of_ocaml
open Alg
open Term

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

let parse_rs syms rules =
  let s = syms in
  let s = String.split_on_char ',' s in
  let s = List.map String.trim s in
  let s = List.map (fun s -> String.split_on_char ':' s) s in
  let w = ref 0 in
  let s = List.map (function (f::n::_) -> decr w; Op.make ~weight:(!w) f (int_of_string n) | _ -> failwith "unknown arity") s in
  ParserRefs.syms := s;
  let rs = Parser.main Lexer.token (Lexing.from_string rules) in
  let name =
    let n = ref 0 in
    fun () -> incr n; "R<sub>" ^ string_of_int !n ^ "</sub>"
  in
  let rs = List.map (fun (s,t) -> RS.Rule.make (name ()) s t) rs in
  RS.make s rs

(** Replace [c] by [t] in [s]. *)
let rec replace c t s =
  try
    let n = String.index s c in
    replace c t (String.sub s 0 n ^ t ^ String.sub s (n+1) (String.length s-(n+1)))
  with
  | Not_found -> s

let run _ =
  let syms = jsget (Html.CoerceTo.textarea (jsget (doc##getElementById(Js.string "symbols")))) in
  let rules = jsget (Html.CoerceTo.textarea (jsget (doc##getElementById(Js.string "rules")))) in
  let completion = jsget (doc##getElementById(Js.string "completion")) in
  let squier = jsget (doc##getElementById(Js.string "squier")) in
  let go = jsget (doc##getElementById(Js.string "go")) in
  let status = jsget (doc##getElementById(Js.string "status")) in
  let status s = status##.innerHTML := Js.string s in
  let error s = status ("<em style=\"color:red\">" ^ s ^ "</em>") in

  go##.onclick :=
    Html.handler
      (fun _ ->
         try
           status "Started computation...";
           let syms = Js.to_string syms##.value in
           let rules = Js.to_string rules##.value in

           status "Parsing rewriting system...";
           let rs = parse_rs syms rules in
           status ("Parsed: " ^ RS.to_string rs);

           status "Computing Knuth-Bendix completion...";
           let display rs =
             completion##.innerHTML := Js.string (replace '\n' "<br/>" (RS.to_string ~var:Var.namer_natural rs))
           in
           let namer =
             let n = ref 0 in
             fun () -> incr n; Printf.sprintf "K<sub>%d</sub>" !n
           in
           let rs = RS.knuth_bendix ~namer ~callback:display rs in
           display rs;

           status "Computing Squier completion...";
           let sq = RS.squier rs in
           let sqs =
             let rule_name = Utils.namer (fun (s1,s2) (s1',s2') -> RS.Path.eq s1 s1' && RS.Path.eq s2 s2') in
             let ans = ref "" in
             List.iter
               (fun (s1,s2) ->
                  let n = rule_name (s1,s2) + 1 in
                  let var = Var.namer_natural () in
                  ans := Printf.sprintf "%s<h3>C<sub>%d</sub></h3><p>%s<br/>%s</p>%!" !ans n (RS.Path.to_string ~var s1) (RS.Path.to_string ~var s2)
               ) sq;
             !ans
           in
           squier##.innerHTML := Js.string sqs;

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
