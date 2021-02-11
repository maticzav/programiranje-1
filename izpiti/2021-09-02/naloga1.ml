let ( >> ) f g x = g (f x)

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki za trojico celih števil preveri ali tvorijo
  pitagorejsko trojico. Trojica [(a, b, c)] je pitagorejska, če je [a^2 + b^2]
  enako [c^2].

    pitagorejska_trojica : int * int * int -> bool

    # pitagorejska_trojica (3, 4, 5);;
    - : bool = true
    # pitagorejska_trojica (5, 4, 3);;
    - : bool = false

[*----------------------------------------------------------------------------*)
let pitagorejska_trojica a b c = (a * a) + (b * b) = c * c

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki za celo število [x] vrne celo število [a], kjer velja,
  da koren števila [x] leži na intervalu [a, a+1).

    priblizek_korena : int -> int

      # priblizek_korena 9;;
    - : int = 3
    # priblizek_korena 17;;
    - : int = 4

[*----------------------------------------------------------------------------*)

let priblizek_korena = Float.of_int >> sqrt >> floor >> Int.of_float

(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme seznam celih števil in najprej IZPIŠE vsa
  soda števila v seznamu, nato pa IZPIŠE še vsa liha števila v seznamu.
  Števila naj bodo izpisana v isti vrstici in med njimi ne želimo presledkov.

    izpisi_soda_liha : int list -> unit

    # izpisi_soda_liha [3; 1; 4; 1; 5; 9; 2];;
    4231159- : unit = ()
    # izpisi_soda_liha [2; 7; 1; 8; 2; 8; 1];;
    2828711- : unit = ()

[*----------------------------------------------------------------------------*)

let je_sodo a = a mod 2 = 0

let je_liho = je_sodo >> not

let izpisi_soda_liha ns =
  let soda = List.filter je_sodo ns in
  let liha = List.filter je_liho ns in
  (* Pisatelj *)
  let izpisi = Int.to_string >> print_string in
  List.iter izpisi soda;
  List.iter izpisi liha

(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki sprejme seznam elementov tipa [option] in preveri, da
  si v seznamu izmenično sledijo konstruktorji [None] in [Some].

    alternirajoci_konstruktorji : 'a option list -> bool

    # alternirajoci_konstruktorji [None; Some 1; None; Some 100; None];;
    - : bool = true
    # alternirajoci_konstruktorji [None; Some 1; Some 10];;
    - : bool = false
    # alternirajoci_konstruktorji [Some 1; None; Some 10; None];;
    - : bool = true

[*----------------------------------------------------------------------------*)

let alternira a b =
  match (a, b) with Some _, None | None, Some _ -> true | _ -> false

let rec alternirajoci_konstruktorji = function
  | [] | [ _ ] -> true
  | a :: b :: xs -> alternira a b && alternirajoci_konstruktorji (b :: xs)

(* e *)
(*----------------------------------------------------------------------------*]
  Funkcija [najmanjsi_rezultat] naj za element [x] in seznam funkcij [fs] vrne
  INDEKS funkcije, ki ima pri argumentu [x] najmanjšo vrednost izmed vseh
  funkcij v seznamu [fs]. Ker je seznam morda prazen, naj bo rezultat tipa
  [option].

  Za vse točke naj bo funkcija repno rekurzivna.

    najmanjsi_rezultat : 'a -> ('a -> 'b) list -> int option

    # najmanjsi_rezultat (-10) [(fun x -> 10 * x); succ; (fun y -> -10 * y)];;  
    - : int option = Some 0
    # najmanjsi_rezultat 10 [(fun x -> 10 * x); succ; (fun y -> -10 * y)];;    
    - : int option = Some 2
    # najmanjsi_rezultat 30 [];;
    - : int option = None

[*----------------------------------------------------------------------------*)

let najmanjsi_rezultat a fs =
  let rec aux (i : int) (acc : (int * 'a) option) = function
    | [] -> acc
    | f :: xs -> (
        let vrednost = f a in
        match acc with
        | Some (i_najmanjsa, vrednost_najmanjsa) ->
            (* Če je vrednost manjša popravi podatek, drugače ohrani zadnjega. *)
            if vrednost_najmanjsa > vrednost then
              aux (i + 1) (Some (i, vrednost)) xs
            else aux (i + 1) acc xs
        | None ->
            (* Če nimamo še najmanjše vrednosti, vrni trenutno. *)
            aux (i + 1) (Some (i, vrednost)) xs )
  in
  aux 0 None fs |> Option.map fst
