let () =
  let relations = ref "" in
  Arg.parse [] (fun s -> relations := s) "Usage: bergman [options]"
