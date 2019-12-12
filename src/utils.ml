(** Give integer names. *)
let namer eq =
  let n = ref 0 in
  let names = ref [] in
  fun e ->
    if not (List.exists (fun (e',_) -> eq e e') !names) then
      (
        names := (e,!n) :: !names;
        incr n
      );
    snd (List.find (fun (e',_) -> eq e e') !names)
      
