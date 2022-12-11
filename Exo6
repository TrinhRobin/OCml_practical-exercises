type arbre = Feuille of char | Noeud of arbre*arbre;;

let arbre_test =
	let arbre_g =Noeud(Noeud(Feuille `m`,Feuille`a`),Feuille`o`) 
	and arbre_d =  Noeud(Feuille`c` ,Noeud(Feuille`l` , Feuille ` `)) in
	Noeud(arbre_g, arbre_d);; 

exception Lire;;
let rec lire a li = match a,li with
|Noeud_,[] -> raise Lire
|Noeud (_,d), true::q -> lire d q
|Noeud (g,_), false::q -> lire g q 
|Feuille`c`, _ -> `c`,li;;


let decode a li = 
let rec aux li accu = match li with 
|[] -> accu
|_-> let c,reste =lire a li in aux a reste (c::acu)
and convertit li str i= match li with
|[] -> str
|x::q -> str.[i] <- x;convertit q str (i-1) in let n= liste_length li in let str= make string n ` ` in convertit li str (n-1);;  

let rec insere x li ordre = match li with 
|[] -> [x]
|y::q when ordre y x -> let l=insere x q ordre in y::l
|li-> x::li ;;

let rec tri li ordre = match li with
|[] -> []
|x::q -> insere x (tri q ordre) ordre;;

let liste_occu str = 
let rec remplit t str i n= match i with
|i when i=n -> t
|_ -> let k = int_of_char str.[i] in
 t.(k) <- t.(k) +1, remplit t str (i+1) n
and recupere t i accu = match i with
|i when i=vect_length t -> accu
|i when t.(i)=0 -> recupere t (i+1) accu
|_ -> recupere t (i+1) ((Feuille (char_of_int i),t;(i))::accu)
in
let t_ini=make_vect 256 0 in  




































 

