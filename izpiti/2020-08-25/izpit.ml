(* 1. naloga *)
let vector_size (x, y) = sqrt ((x *. x) +. (y *. y))

let dot_product (x1, y1) (x2, y2) = (x1 *. x2) +. (y1 *. y2)

let angle_between v1 v2 =
  acos (dot_product v1 v2 /. (vector_size v1 *. vector_size v2))

let list_to_triple = function [ a; b; c ] -> Some (a, b, c) | _ -> None

type counter = { lt : int; eq : int; gt : int }

let compare_with list a =
  let init = { lt = 0; eq = 0; gt = 0 } in

  (* Primerjaj velikost in prištej kamor mora it. *)
  let aux acc el =
    if el < a then { lt = acc.lt + 1; eq = acc.eq; gt = acc.gt }
    else if el = a then { lt = acc.lt; eq = acc.eq + 1; gt = acc.gt }
    else { lt = acc.lt; eq = acc.eq; gt = acc.gt + 1 }
  in
  List.fold_left aux init list

let apply_all fs init =
  let rec aux acc = function [] -> acc | g :: gs -> aux (g acc) gs in
  aux init fs

(* 2. naloga *)

type xytree =
  | Xsplit of int * xytree * xytree
  | Ysplit of int * xytree * xytree
  | Elements of (int * int) list

let x x (lt, rt) = Xsplit (x, lt, rt)

let y y (lt, rt) = Ysplit (y, lt, rt)

let el ps = Elements ps

let primer =
  x 2
    ( y 3 (el [ (0, 2); (1, 1) ], el []),
      y 2 (el [ (3, 1) ], x 4 (el [ (4, 3) ], el [])) )

let rec num_of_elements = function
  (* Seštej poddrevesa. *)
  | Xsplit (_, lt, rt) | Ysplit (_, lt, rt) ->
      num_of_elements lt + num_of_elements rt
  (* Vrni število elementov *)
  | Elements els -> List.length els

let rec insert (px, py) = function
  (* Nakoncu samo dodamo v seznam *)
  | Elements points -> Elements ((px, py) :: points)
  (* Pogledamo v katero delitev je treba točko vstavit, jo tja vstavimo in vrnemo novo
     delilno drevo z novo vejo. *)
  | Xsplit (line, lt, rt) ->
      if px <= line then Xsplit (line, insert (px, py) lt, rt)
      else Xsplit (line, lt, insert (px, py) rt)
  (* Enako v Y primeru, le da tokrat gledamo y-koordinato. *)
  | Ysplit (line, lt, rt) ->
      if py <= line then Ysplit (line, insert (px, py) lt, rt)
      else Ysplit (line, lt, insert (px, py) rt)

let is_alternate a b =
  (* Drevo ne alternira samo če X-u sledi X, oziroma Y-u Y.
     Če sledi kateremukoli Elements alternira saj se drevo konča,
     če sta različna pa tudi alternirata. *)
  match (a, b) with
  | Xsplit _, Xsplit _ -> false
  | Ysplit _, Ysplit _ -> false
  | _ -> true

let rec alternates = function
  (* Drevo z elementi očitno alternira. *)
  | Elements _ -> true
  (* Če imamo večje drevo pogledamo ali to koleno alternira in
     se rekurzivno zapeljemo navzdol po drevesu, če drži. *)
  | (Xsplit (_, lt, rt) as x) | (Ysplit (_, lt, rt) as x) ->
      is_alternate x lt && is_alternate x rt && alternates lt && alternates rt

(* Boxed correctly *)
(* 
Bottom-up we accumulate a list of points so far and tell 
whether they are in the right position.
*)

(* Pomožna funkcija, ki iz dvojca vrne koordinato glede na drevo. *)
let coord tree (x, y) =
  match tree with
  | Xsplit _ -> x
  | Ysplit _ -> y
  | Elements _ -> failwith "Can't get component of elements!"

let boxed_correctly tree =
  (* Pomožne funkcije *)
  let rec vse_levo x = function
    | [] -> true
    | px :: xs -> px <= x && vse_levo x xs
  in
  let rec vse_desno x = function
    | [] -> true
    | px :: xs -> x < px && vse_desno x xs
  in
  (* Iterator *)
  let rec aux = function
    | Elements els -> (els, true)
    | (Xsplit (l, lt, rt) | Ysplit (l, lt, rt)) as split ->
        (* Pogledamo ali so do sedaj vrednosti pravilno razvrščene. *)
        let l_ps, sub_l_correct = aux lt in
        let r_ps, sub_r_correct = aux rt in
        (* Pogledamo ali so tudi sedaj pravilno ravrščene tako,
           da gremo čez vse točke in gledamo meje. *)
        if sub_l_correct && sub_r_correct then
          (* Izračunamo kako jemljemo koordinate. *)
          let cord = coord split in
          (* Pogledamo ali so točke na pravih mestih. *)
          let l_correct = vse_levo l (List.map cord l_ps) in
          let r_correct = vse_desno l (List.map cord r_ps) in
          (l_ps @ r_ps, l_correct && r_correct)
        else ([], false)
  in

  let _, correct = aux tree in
  correct

let napacen_primer =
  x 2
    ( y 3 (el [ (5, 2); (1, 1) ], el []),
      y 2 (el [ (3, 1) ], x 4 (el [ (4, 3) ], el [])) )
