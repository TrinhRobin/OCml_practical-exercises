
type 'a arbre = Vide | Noeud of 'a arbre * 'a * 'a arbre;;

let rec recherche x a = match a with
|Vide -> false
|Noeud(g,y,d) when x=y -> true
|Noeud(g,y,d) when x<y -> recherche x g
|Noeud(g,y,d) -> recherche x d;;

let rec insere x a = match a with
|Vide -> Noeud(Vide,x,Vide)
|Noeud(g,y,d) when x=y ->  Noeud(g,y,Noeud(Vide,x,d))
|Noeud(g,y,d) when x<y -> Noeud(insere x g,y,d)
|Noeud(g,y,d) -> Noeud(g,y,insere x d) ;;

let rec suppr_max a = match a with
|Vide -> failwith"liste vide"
|Noeud(g,y,Vide) -> g, y
|Noeud(g,y,d) -> let x,y = (suppr_max d) in Noeud(g,y,x), y;;

let rec suppr x a = match a with
|Vide -> Vide
|Noeud(g,y,d) when y=x -> let x,z = suppr_max g in Noeud(x,z,d)
|Noeud(g,y,d) when y>x -> Noeud(suppr x g,y,d)
|Noeud(g,y,d) -> Noeud(g,y,suppr x d);;
 

let rec fusion a1 a2 = match a1  with
|Vide -> a2
|Noeud(g,x,Vide) -> Noeud(g,x,a2)
|Noeud(g,x,d) -> Noeud(g,x,fusion d a2);;

let rec suppr_2 x a = match a with
|Vide -> Vide
|Noeud(g,y,d) when y=x -> fusion g d
|Noeud(g,y,d) when y>x -> Noeud(suppr_2 x g,y,d)
|Noeud(g,y,d) -> Noeud(g,y,suppr_2 x d);;
 
let list_of_arbre a =
let rec aux a accu = match a with
|Vide -> accu
|Noeud(g,x,d) -> let li= x::(aux d accu) in aux g li in aux a [];;

let rec arbre_of_list li = match li with
|[] -> Vide
|x::q -> insere x (arbre_of_list q);;

let rec list_of_arbre_2 a = match a with
| Vide -> []
| Noeud (g, x, d) -> (list_of_arbre_2 g) @ [x] @ (list_of_arbre_2 d);;

let tri li = list_of_arbre (arbre_of_list li) ;;

let bit n k = match k with 
  | 0 -> n mod 2 
  | _ -> bit (n / 2) (k - 1);
 
let log_entier li =
  let rec max_liste li = match li with 
    | [] -> failwith "Liste vide"
    | [x] -> x 
    | x :: y :: q when x > y -> max_liste (x :: q)
    | x :: y :: q -> max_liste (y :: q) in 
  let rec log_aux m k = match m with    
    | 0 -> k 
    | _ -> log_aux (m / 2) (k + 1) in
  let m = max_liste li in 
  log_aux m 0;;



