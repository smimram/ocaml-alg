(* Testing Joyal's theta category. *)

open Alg

module T = Category.Theta
module PS = PastingScheme
module S = Category.Simplicial

(** Lists of n natural numbers summing up to total: those are the simplicial morphisms. *)
let rec compositions total n =
  if n = 0 then if total = 0 then [[]] else []
  else
    List.init (total + 1) Fun.id
    |> List.concat_map (fun k -> List.map (List.cons k) (compositions (total - k) (n - 1)))

(** Cartesian product of lists. *)
let rec product = function
  | [] -> [[]]
  | l::ll ->
    let r = product ll in
    List.concat_map (fun x -> List.map (List.cons x) r) l

(** All the morphisms between two pasting schemes, computed from the definition and thus independently of composition. *)
let rec hom p q =
  let m = PS.width p in
  let n = PS.width q in
  compositions (m + 1) (n + 1)
  |> List.concat_map
    (fun s ->
       let f = S.ap s in
       (* The least i such that j ≤ f(i). *)
       let index j =
         let rec aux i = if j <= f i then i else aux (i + 1) in
         aux 0
       in
       List.init (f m - f 0) (fun j -> f 0 + j + 1)
       |> List.map (fun j -> hom (List.nth (PS.to_list p) (index j - 1)) (List.nth (PS.to_list q) (j - 1)))
       |> product
       |> List.map (T.E.make p q s))

let point = PS.point
let d1 = PS.susp point
let d2 = PS.susp d1
let d3 = PS.susp d2
let c2 = PS.PS [point; point]
let c3 = PS.PS [point; point; point]
let whisker = PS.PS [d1; point]
let hcomp = PS.PS [d1; d1]

(** The morphisms p → q are the cells of shape p in the free ω-category on q, which we have counted by hand. *)
let () =
  List.iter
    (fun (p, q, n) ->
       let n' = List.length (hom p q) in
       Printf.printf "|Hom(%s,%s)| = %d\n" (PS.to_string p) (PS.to_string q) n';
       assert (n' = n))
    [
      point, point, 1;
      point, d1, 2;
      d1, d1, 3;
      point, d2, 2;
      d1, d2, 4;
      d2, d2, 5;
      d2, d1, 3;
      point, c2, 3;
      d1, c2, 6;
      c2, d1, 4;
    ]

(** Units: on all the morphisms between the schemes below. *)
let () =
  let schemes = [point; d1; d2; d3; c2; c3; whisker; hcomp] in
  List.iter
    (fun p ->
       List.iter
         (fun q ->
            List.iter
              (fun f ->
                 assert (T.E.eq (T.comp (T.id p) f) f);
                 assert (T.E.eq (T.comp f (T.id q)) f))
              (hom p q))
         schemes)
    schemes

(** Composites are morphisms of the expected type and composition is associative: on all the composable morphisms between the schemes below. *)
let () =
  let schemes = [point; d1; d2; c2; whisker] in
  let hom = List.concat_map (fun p -> List.map (fun q -> (p, q), hom p q) schemes) schemes in
  let hom p q = List.assoc (p, q) hom in
  let count = ref 0 in
  List.iter
    (fun p ->
       List.iter
         (fun q ->
            List.iter
              (fun r ->
                 List.iter
                   (fun f ->
                      List.iter
                        (fun g ->
                           let fg = T.comp f g in
                           assert (PS.eq (T.src fg) p);
                           assert (PS.eq (T.tgt fg) r);
                           assert (List.exists (T.E.eq fg) (hom p r));
                           List.iter
                             (fun s ->
                                List.iter
                                  (fun h ->
                                     incr count;
                                     assert (T.E.eq (T.comp fg h) (T.comp f (T.comp g h))))
                                  (hom r s))
                             schemes)
                        (hom q r))
                   (hom p q))
              schemes)
         schemes)
    schemes;
  Printf.printf "Checked associativity on %d triples.\n" !count
