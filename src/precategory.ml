(** Higher-dimensional precategories. *)

(** Free precategories. *)
module Make (X : Alphabet.T) = struct
  (** A cell. *)
  (* Note: we suppose that generators are whiskered (by identities if necessary)
  all the way down. *)
  type cell =
    | C of whisker * cell (** a composite *)
    | I of cell (** an identity *)

  (** A whiskered generator. *)
  and whisker =
    | G of X.t (** a generator *)
    | W of cell * whisker * cell (** a whisker context *)

  type t = cell

  (** String representation of a cell. *)
  let to_string c =
    let rec list = function
      | C (w,c) -> w::(list c)
      | I _ -> []
    in
    let rec cell = function
      | C _ as c -> "[" ^ String.concat "|" (List.map whisker (list c)) ^ "]"
      | I c -> "[" ^ cell c ^ "]"
    and whisker = function
      | G g -> X.to_string g
      | W (c1,w,c2) -> cell c1 ^ whisker w ^ cell c2
    in
    cell c

  (** Dimension of a cell. *)
  let dim c =
    let rec cell = function
      | C (w,_) -> whisker w
      | I c -> cell c + 1
    and whisker = function
      | G g -> 0
      | W (_,w,_) -> whisker w + 1
    in
    cell c

  (** Composition of two cells on the boundary of maximal dimension. *)
  let rec compose c d =
    if dim c = dim d then
      let rec aux = function
        | C (w,c) -> C (w, aux c)
        | I _ -> d
      in
      aux c
    else if dim c > dim d then
      let rec aux = function
        | C (w,c) ->
           let w =
             match w with
             | W (c1,w,c2) -> W (c1,w,compose c2 d)
             | G _ -> assert false
           in
           let c = aux c in
           C (w,c)
        | I c -> I (compose c d)
      in
      aux c
    else
      (*
      let rec aux = function
        | C (w,d) ->
           let w =
             match w with
             | W (d1,w,d2) -> W
           in
           let d = aux d in
           C (w,d)
        | I d -> I (compose c d)
      in
      aux d
       *)
      failwith "TODO"

  (** Presentation of a precategory: a "pre-polygraph". *)
  module Pres = struct
    module E = Map.Make(X)

    (* By convention, a zero cell has (physically) itself as source and target. *)
    type t = (cell * cell) E.t

    (** Empty signature. *)
    let empty = E.empty

    (** Does a generator belong the signature? *)
    let mem s g =
      E.mem g s

    let boundary s g =
      let x,y = E.find g s in
      assert (x != g && y != g);
      x,y

    (** Source of a generator. *)
    let source s g =
      fst (boundary s g)

    (** Target of a generator. *)
    let target s g =
      snd (boundary s g)

    (** Operations on cells in a presentation. *)
    module Cell = struct
      (** Ensure that a cell uses only generators defined in the signature. *)
      let mem s c =
        let rec cell = function
          | C (w,c) -> whisker w && cell c
          | I c -> cell c
        and whisker = function
          | W (c1,w,c2) -> cell c1 && whisker w && cell c2
          | G g -> mem s g
        in
        cell c

      (** Dimension of a cell. *)
      let dim s c = dim c

      (* let source s c = *)
        (* let rec cell = function *)
          (* | C (w,c) -> whisker w *)
          (* | I c -> c *)
        (* and whisker = function *)
          (* | W (c1,w,c2) -> *)
             (* cell  *)
          (* | G g -> *)
        (* in *)
    end
  end
end
