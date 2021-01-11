(* 1. naloga *)

let odstej_trojici (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

let max_rezultat_do_n f n =
  (* Začnemo z najmanjšo vrednostjo (f 0) in stopamo do konca *)
  let rec aux acc i = if i <= n then aux (max (f n) acc) (n + 1) else acc in
  aux (f 0) 1

let najvecji = function
  | [] -> failwith "Prazen seznam"
  | x :: xs -> List.fold_left max x xs

let max_rezultat_do_n_2 f n = List.init n f |> najvecji

let pocisti_seznam oxs =
  let rec aux acc = function
    | [] -> List.rev acc
    | Some x :: xs -> aux (x :: acc) xs
    | None :: xs -> aux acc xs
  in
  aux [] oxs

let rec narasca = function
  | [] | [ _ ] -> true
  | a :: b :: xs -> a <= b && narasca (b :: xs)

let rec pada = function
  | [] | [ _ ] -> true
  | a :: b :: xs -> a >= b && pada (b :: xs)

let sodo = function x when x mod 2 = 0 -> Some x | _ -> None

let liho = function x when x mod 2 = 0 -> None | x -> Some x

let preveri_urejenost xs =
  let liha = xs |> List.map liho |> pocisti_seznam in
  let soda = xs |> List.map sodo |> pocisti_seznam in
  narasca soda && pada liha

(* 2. naloga *)

type 'a gnezdenje = Element of 'a | Podseznam of 'a gnezdenje list

let el a = Element a

let sez ls = Podseznam ls

let gnezdenje_primer =
  sez [ el 1; el 2; sez [ el 3; sez [ el 4 ]; sez [] ]; sez [ el 5 ] ]

let rec najvecja_globina = function
  | Element _ -> 0
  | Podseznam [] -> 1
  | Podseznam xs -> 1 + (List.map najvecja_globina xs |> najvecji)

let rec preslikaj f = function
  | Element a -> Element (f a)
  | Podseznam xs -> Podseznam (List.map (preslikaj f) xs)

let rec splosci = function
  | Element a -> [ a ]
  | Podseznam xs -> List.map splosci xs |> List.fold_left ( @ ) []

let nasprotno a b =
  match (a, b) with
  | Element _, Podseznam _ | Podseznam _, Element _ -> true
  | _ -> false

let rec alternirajoci_konstruktorji = function
  | [] | [ _ ] -> true
  | x :: y :: xs -> nasprotno x y && alternirajoci_konstruktorji (y :: xs)

let podseznam = function
  | Podseznam xs -> xs
  | Element _ -> failwith "Pričakoval sem Element."

let rec fold f acc = function
| [] -> []
| x :: xs -> fold f (f acc x) xs

let rec fold_gnezdenje f acc = function
| Element a -> f acc a
| Podseznam xs -> fold (fold_gnezdenje f) acc xs

let rec fold_g f acc = function
| Element a -> f acc a
| Podseznam [] -> acc
| Podseznam (Element x :: xs) -> fold_g f (f acc x) (Podseznam xs)
(* In tukaj ni repno rekurzivno. *)
| Podseznam (Podseznam x :: xs) -> fold_g f (fold_g f acc (Podseznam x)) (Podseznam xs)

(* sez [ sez [ el 3; sez [ el 4 ]; sez [] ]; ] *)



let zlozi_preko_gnezdenja f acc xs = fold_gnezdenje f acc (Podseznam xs)



let rec splosci = function
  | Element a -> [ a ]
  | Podseznam xs -> List.map splosci xs |> List.fold_left ( @ ) []

(* 3. naloga *)

let zbezi = 0