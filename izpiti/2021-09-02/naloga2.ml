(*============================================================================*]
  Za učinkovitejše iskanje po leksikografsko urejenih parih bomo uporabili
  leksikografska drevesa, ki jih ustvarimo s pomočjo dvojiških dreves.

    type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  Leksikografsko drevo za pare tipa ['a * 'b] je dvojiško drevo, ki ima v
  vozlišču element tipa ['a] (da lahko primerjamo po prvi komponenti) in pa
  drevo tipa ['b tree] (za primerjanje po drugi komponenti).

    type ('a, 'b) lexi_tree = ('a * 'b tree) tree

  Par [(a, b)] se nahaja v leksikografskem drevesu, če imamo v drevesu vozlišče
  s parom [(a, subtree)] in se [b] nahaja v [subtree]. 

  Primer drevesa za pare (3, "g"), (3, "t"), (7, "a"), (10, "e"), (10, "r"),
  (10, "t") in (10, "z") je:
          
          (7)--------┐
           |   "a"   |
           └---------┘
          /           \
         /             \
    (3)-------┐     (10)-----------┐
     | "g"    |      |     "r"     |
     |    \   |      |    /   \    |
     |    "t" |      |  "e"   "z"  |
     └--------┘      |       /     |
                     |     "t"     |
                     └-------------┘

[*============================================================================*)

let ( >> ) f g x = g (f x)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type ('a, 'b) lexi_tree = ('a * 'b tree) tree

(* a *)
(*============================================================================*]
  Definirajte primer, ki ustreza zgornjemu leksikografskemu drevesu.

[*============================================================================*)
let leaf a = Node (Empty, a, Empty)

let primer : (int, string) lexi_tree =
  (* Poddrevesa *)
  let sedem = (7, leaf "a") in
  let tri = (3, Node (Empty, "g", leaf "t")) in
  let deset = (10, Node (leaf "e", "r", Node (leaf "t", "z", Empty))) in
  (* Drevo *)
  Node (leaf tri, sedem, leaf deset)

(* b *)
(*============================================================================*]
  Napišite funkcijo, ki preveri ali je par prisoten v leksikografskem drevesu.
[*============================================================================*)

let rec vsebuje (a, b) drevo =
  match drevo with
  (* Če je drevo prazno, zagotovo ne vsebuje niz. *)
  | Empty -> false
  (* Če drevo ni prazno, najprej gledamo ali vsebuje prvo komponento, nato pa še drugo. *)
  | Node (l, (x, yt), d) ->
      (* Ker je drevo leksikografsko urejeno pogledamo v kakšnem razmerju sta člena. *)
      if a = x then
        let rec pod_vsebuje = function
          | Empty -> false
          | Node (l, m, d) ->
              if m = b then true
              else if x < a then pod_vsebuje l
              else pod_vsebuje d
        in
        pod_vsebuje yt
      else if a < x then vsebuje (a, b) l
      else vsebuje (a, b) d

(* c *)
(*============================================================================*]
  Napišite funkcijo za vstavljanje elementov v leksikografsko drevo.
[*============================================================================*)

let rec vstavi (a, b) (drevo : ('a, 'b) lexi_tree) : ('a, 'b) lexi_tree =
  match drevo with
  (* Če je drevo prazno, dodamo element kot list. *)
  | Empty -> leaf (a, leaf b)
  (* Če drevo ni prazno, pogledamo ali mora desno ali levo. *)
  | Node (l, (x, yt), d) ->
      (* Če sta prva člena enaka, vstavimo v to poddrevo. *)
      if a = x then
        let rec pod_vstavi = function
          | Empty -> leaf b
          | Node (l, m, d) ->
              if b <= m then Node (pod_vstavi l, m, d)
              else Node (l, m, pod_vstavi d)
        in
        Node (l, (x, pod_vstavi yt), d)
      else if a < x then Node (vstavi (a, b) l, (x, yt), d)
      else Node (l, (x, yt), vstavi (a, b) d)

(* d *)
(*============================================================================*]
  Napišite funkcijo [lexi_fold], ki sprejme funkcijo [f] in začetno vrednost
  akumulatorja, nato pa funkcijo zloži preko leksikografskega drevesa. Vrstni
  red zlaganja je določen z leksikografsko urejenostjo.

    lexi_fold : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) lexi_tree -> 'a
[*============================================================================*)

let rec lexi_fold f acc drevo =
  match drevo with
  | Empty -> acc
  | Node (l, (a, bt), d) ->
      (* Najprej foldamo po levem drevesu. *)
      let l_acc = lexi_fold f acc l in
      (* Nato po vrednostih. *)
      let new_acc =
        (* Podfold, ki gre čez elemente v drugem drevesu. *)
        let rec tree_fold acc = function
          | Empty -> acc
          | Node (l, t_a, d) ->
              let l_acc = tree_fold acc l in
              let kriz = f l_acc a t_a in
              tree_fold kriz d
        in
        tree_fold l_acc bt
      in
      (* Nakoncu pa se po desni veji. *)
      lexi_fold f new_acc d

(* e *)
(*============================================================================*]
  Napišite funkcijo, ki vrne urejen seznam vseh elementov, ki se nahajajo v
  leksikografskem drevesu.
[*============================================================================*)

let elementi = lexi_fold (fun acc a b -> (a, b) :: acc) [] >> List.rev
