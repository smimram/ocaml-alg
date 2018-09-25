open Term

let m = Op.make "m" 2
let e = Op.make "e" 0
let e = app e [||]
let x = var ()
let y = var ()
let z = var ()
let assoc_l = app m [|app m [|x;y|];z|]
let assoc_r = app m [|x; app m [|y;z|]|]
let unit_l = app m [|e; x|]
let unit_c = x
let unit_r = app m [|x; e|]
let rs = [
    RS.Rule.make "assoc" assoc_l assoc_r;
    RS.Rule.make "unit-l" unit_l unit_c;
    RS.Rule.make "unit-r" unit_r unit_c
  ]

let t = app m [|app m [|app m [|x; y|]; z|]; var ()|]

let () =
  Printf.printf "t: %s\n%!" (to_string t);
  Printf.printf "t^: %s\n\n%!" (RS.Path.to_string (RS.normalize rs t));
  List.iter
    (fun (s1,s2) ->
      Printf.printf "cp : %s\n     %s\n\n%!" (RS.Step.to_string s1) (RS.Step.to_string s2)
    ) (RS.critical rs)
