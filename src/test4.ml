module X = Alphabet.String
module CP = Category.Pres(X)(X)

let () =
  let p = CP.empty in
  let p = CP.add_object p "x" in
  let p = CP.add_object p "y" in
  let p = CP.add_object p "z" in
  let p = CP.add_morphism p "f" "x" "y" in
  let p = CP.add_morphism p "g" "y" "z" in
  let p = CP.add_morphism p "h" "x" "z" in
  let p = CP.add_relation p (CP.Free.comp (CP.morphism p "f") (CP.morphism p "g")) (CP.morphism p "h") in
  Printf.printf "%s\n%!" (CP.to_string p)
