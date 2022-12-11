


let min_liste li = 
let rec min_aux li = match li with
|[]-> failwith "liste vide"
|[x]-> x
|x::q-> min x (min_aux q)
in let rec creer_liste n li accu = match li with
|[] -> failwith "liste vide"
|x::q when x=n -> n,accu@q
|x::q -> creer_liste n q (x::accu) in creer_liste (min_aux li) li [];;




let tri_selection li=
	let rec aux li accu = match li with
	|[]-> failwith "liste vidde"
	|[x]-> x::accu
	|x::q-> let a,y=min_liste li in aux y (a::accu) in rev (aux li []);;

tri_selection [1; 7; 5];;


let separe li=
	let rec separe_aux li li1 li2=match li with
	|[]-> li1,li2
	|[x]-> (x::li1),li2
	|x::y::q -> separe_aux q (x::li1) (y::li2) in separe_aux li [] [];;


let fusion li1 li2=
	let rec fusion_aux li1 li2 accu= match li1, li2 with
	|[],_ -> li2@accu
	|_,[] -> li1@accu
	|x::q,y::r when x<y -> fusion_aux q li2 (x::accu)
	|x::q,y::r -> fusion_aux li1 r (y::accu) in rev(fusion_aux li1 li2 []);;



let rec tri_fusion li= match li with
|[]-> []
|[x]-> [x]
|li-> let li1,li2= separe li in let x=tri_fusion li1 and y=tri_fusion li2 in fusion x y;;


let partition x li=
	let rec partition_aux x li accu1 accu2= match li with
	|[]->accu1 accu2
	|y::q when y>x -> partition_aux x q accu1 (y::accu2)
	|y::q -> partition_aux x q (y::accu1) accu2 in partition_aux x li [] []
partition [1;2;7;8;4;6];;
(*
let rec tri_rapide li=
|[]->[]
|t::q -> let li1,li2 = partition t q in let x,y = tri_rapide li1 , tri_rapide li2 in x@t::y

let rec bit n k = match k with
|0-> 
|k-> bit (n/2) (k-1)

*)


