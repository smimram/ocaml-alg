(** Automata. *)

(** Regular expressions. *)
module Regexp (X : Alphabet.T) = struct
  type t =
    | Letter of X.t
    | Union of t * t
    | Empty
    | Concat of t * t (** concatenation *)
    | Singl (** empty word singleton *)
    | Star of t

  let letter a = Letter a
  let union r s = Union (r,s)
  let empty = Empty
  let concat r s = Concat (r,s)
  let star r = Star r

  let rec unions = function
    | [] -> Empty
    | [r] -> r
    | r::l -> union r (unions l)

(*
  let rec to_string = function
    | Letter a -> X.to_string a
    | Union (r,s) -> "(" ^ to_string r ^ ") + (" ^ to_string s ^ ")"
    | Empty -> "0"
    | Concat (r,s) -> "(" ^ to_string r ^ ")(" ^ to_string s ^ ")"
    | Singl -> "1"
    | Star r -> "(" ^ to_string r ^ ")*"
*)
                    
  let to_string r =
    (* level: 0:+ / 1:x / 2:* *)
    let rec aux l r =
      let pa l' s = if l' < l then "(" ^ s ^ ")" else s in
      match r with
      | Letter a -> X.to_string a
      | Union (r,s) -> pa 0 (aux 0 r ^ "+" ^ aux 0 s)
      | Empty -> "0"
      | Concat (r,s) -> pa 1 (aux 1 r ^ "." ^ aux 1 s)
      | Singl -> "1"
      | Star r -> aux 2 r ^ "*"
    in
    aux (-1) r      

  let rec simpl = function
    | Union (Empty, r) -> simpl r
    | Union (r, Empty) -> simpl r
    | Union (r, s) -> Union (simpl r, simpl s)
    | Concat (Empty, r) -> Empty
    | Concat (r, Empty) -> Empty
    | Concat (Singl, r) -> simpl r
    | Concat (r, Singl) -> simpl r
    | Concat (r, s) -> Concat (simpl r, simpl s)
    | Star Empty -> Empty
    | Star r -> Star (simpl r)
    | Letter _ | Empty | Singl as r -> r

  let simpl r =
    let rec fix f x =
      let y = f x in
      if y = x then x
      else fix f y
    in
    fix simpl r

  module Series = Series.Make(Field.Int)

  (** Generating series of a regular expression. This expression is assumed to
  be unambiguous. *)
  let rec series = function
    | Letter a -> Series.var
    | Union (a, b) -> Series.add (series a) (series b)
    | Empty -> Series.zero
    | Concat (a, b) -> Series.mul (series a) (series b)
    | Singl -> Series.one
    | Star a -> Series.star (series a)
end

module State = Alphabet.Int

module Make (X : Alphabet.T) = struct
  module States = Alphabet.Pow(State)
  module T = Alphabet.Map(Alphabet.Prod(State)(X))(States)
  module Regexp = Regexp(X)

  type t =
    {
      states : int;
      initial : State.t;
      terminal : States.t;
      transitions : T.t;
    }

  let states aut = aut.states

  let trans aut a x =
    try
      T.app aut.transitions (a,x)
    with
    | Not_found -> States.empty

  let add_transition aut a x (b : State.t) =
    let bb = trans aut a x in
    let bb = States.add bb b in
    let transitions = T.add aut.transitions (a,x) bb in
    { aut with transitions }

  let create states initial terminal transitions =
    let terminal = States.of_list terminal in
    let ans = { states; initial; terminal; transitions = T.empty } in
    List.fold_left (fun ans (a,x,b) -> add_transition ans a x b) ans transitions

  let kleene aut =
    let n = states aut in
    let init f = Array.init n (fun i -> Array.init n (fun j -> f i j)) in
    let rr =
      init
        (fun i j ->
          let r = ref Regexp.empty in
          T.iter (fun (i',a) jj ->
              if State.eq i i' && States.mem jj j then r := Regexp.union !r (Regexp.letter a)
            ) aut.transitions;
          if i = j then r := Regexp.union Regexp.empty !r;
          !r
        )
    in
    let rr = ref rr in
    for k = 0 to n - 1 do
      let ss =
        let rr = !rr in
        init
          (fun i j ->
            Regexp.union
              rr.(i).(j)
              (Regexp.concat rr.(i).(k) (Regexp.concat (Regexp.star rr.(k).(k)) rr.(k).(j)))
          )
      in
      rr := ss
    done;
    let rr = !rr in
    let r = ref Regexp.empty in
    let i = aut.initial in
    States.iter (fun j -> r := Regexp.union !r rr.(i).(j)) aut.terminal;
    !r
end
