(* 1. naloga *)

let dot_prod (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let fix_second f value a = f a value

let combine_and_filter f xs ys =
  let rec aux acc = function
    (* Če z enim od seznamov pridemo do konca vrni rezultat obrnjen na glavo. *)
    | [], _ | _, [] -> List.rev acc
    (* Drugače pogledamo ali ju združimo in dodamo na akumulator. *)
    | x :: xs, y :: ys -> (
        match f x y with
        | Some v -> aux (v :: acc) (xs, ys)
        | None -> aux acc (xs, ys) )
  in
  aux [] (xs, ys)

let join = function
  | [] -> ""
  | x :: xs ->
      let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (acc ^ ", " ^ x) xs
      in
      aux x xs

let conditional_print f xs = xs |> List.filter f |> join |> print_string

(* 2. naloga *)

type ('a, 'b) tree =
  | Empty
  | ANode of ('a, 'b) tree * 'a * ('a, 'b) tree
  | BNode of ('a, 'b) tree * 'b * ('a, 'b) tree

let a a = ANode (Empty, a, Empty)

let b b = BNode (Empty, b, Empty)

let test : (int, bool) tree = ANode (b true, 12, ANode (a 0, 5, b false))

let maximum = function
  | [] -> failwith "empty list"
  | x :: xs -> List.fold_left max x xs

let rec a_depth tree =
  let rec aux acc = function
    | Empty -> 0
    | ANode (l, _, d) -> maximum [ aux (acc + 1) l; acc; aux (acc + 1) d ]
    | BNode (l, _, d) -> maximum [ aux (acc + 1) l; aux (acc + 1) d ]
  in
  aux 1 tree

let rec b_depth tree =
  let rec aux acc = function
    (* Če smo prišlo do praznega seznama vrnemo 0 *)
    | Empty -> 0
    (* Če smo prišli do vozlišča pogledamo katera je najglobja
       pojavitev BNode-a med trenutno in vsemi naslednjimi. *)
    | ANode (l, _, d) -> maximum [ aux (acc + 1) l; aux (acc + 1) d ]
    | BNode (l, _, d) -> maximum [ aux (acc + 1) l; acc; aux (acc + 1) d ]
  in
  aux 1 tree

type result = { aNodes : int; bNodes : int }

let count_node node res =
  match node with
  | Empty -> res
  | ANode _ -> { aNodes = res.aNodes + 1; bNodes = res.bNodes }
  | BNode _ -> { aNodes = res.aNodes; bNodes = res.bNodes + 1 }

let rec count = function
  | Empty -> { aNodes = 0; bNodes = 0 }
  | (ANode (l, _, r) | BNode (l, _, r)) as node ->
      let l_count = count l in
      let r_count = count r in
      {
        aNodes = l_count.aNodes + r_count.aNodes;
        bNodes = l_count.bNodes + r_count.bNodes;
      }
      |> count_node node

let rec is_typemirror a b =
  match (a, b) with
  | Empty, Empty -> true
  | ANode (al, a, ad), BNode (bl, b, bd) | BNode (al, a, ad), ANode (bl, b, bd)
    ->
      a = b && is_typemirror al bl && is_typemirror ad bd
  | _ -> false

let rec foldmap fa fb acc = function
  | Empty -> (acc, Empty)
  | ANode (l, x, d) ->
      let new_acc, new_x = fa acc x in
      let _, fold_l = foldmap fa fb new_acc l in
      let _, fold_d = foldmap fa fb new_acc d in
      (new_acc, ANode (fold_l, new_x, fold_d))
  | BNode (l, x, d) ->
      let new_acc, new_x = fb acc x in
      let _, fold_l = foldmap fa fb new_acc l in
      let _, fold_d = foldmap fa fb new_acc d in
      (new_acc, BNode (fold_l, new_x, fold_d))
