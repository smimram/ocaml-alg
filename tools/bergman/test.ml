let () =
  let s = "xy-yxx" in
  Printf.printf "Parsing: %s\n%!" s;
  let p = Parser.main Lexer.token (Lexing.from_string s) in
  Printf.printf "Result : %s\n%!" (Pol.to_string p)
