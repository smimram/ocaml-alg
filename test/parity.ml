(** Testing parity complexes. *)

open Alg

let () =
  for n = 0 to 4 do
    Printf.printf "# %d-cube\n\n%!" n;
    let c = Parity.Cube.make n in
    print_string @@ Parity.Cube.to_string c;
    print_newline ();
    print_newline ()
  done
