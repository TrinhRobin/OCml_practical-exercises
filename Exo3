let max_liste li= 
	let rec aux li accu = match li with
	|[] -> accu
	|x::q when x>accu -> aux q x
	|x::q -> aux q accu in match li with
		|[] -> failwith"liste vide"
		|x::q -> aux q x ;; 

let rec est_sous_liste l1 l2 = match l1,l2 with
|[],q -> true
|x::q,[] -> false
|x::q,y::r when x=y -> est_sous_liste q r 
|x::q,y::r -> est_sous_liste l1 r ;;


let ajoute x ll = 
let rec aux accu x ll = match ll with
|[] -> accu
|li1::q -> aux ((x::li1)::accu) x q in aux [] x ll ;;

ajoute 1 [[1];[1;2]];;

let sous_liste l = 
let rec aux accu l = match l with
|[] -> accu
|x::q -> aux ((ajoute x accu)@accu) q in aux [] l ;;

let long_plslc l1 l2= list_length(intersect l1 l2) ;;

let long_2_plslc l1 l2=
let rec aux l1 l2 accu = match l1,l2 with
|[],[] -> accu
|[],y::r -> accu
|x::q,[] -> accu
|x::q,y::r when x=y -> aux q r accu+1
|x::q,y::r -> let x,y = (aux l1 r accu), (aux q l2 accu) in max x y in aux l1 l2 0 ;; 

let vect_liste li =
let n = list_length li in 
let rec aux tab accu li = match li with
|[] -> tab
|x::q -> tab.(accu) <- x ; aux tab (accu+1) q in 
match li with
|[]-> [||] 
|x::q -> let tab = make_vect n x in aux tab 0 li;;

let long3_plslc li1 li2 = 
let p=list_length li1 and q=list_length li in2 in
let t1=vect_liste li1 and let t2=vect_liste li2 in
let let sol = make_matrix (p+1) (q+1)
for i=p-1 downto 0 do
for j=q-2 downto 0 do
if i=p then sol.(i).(j) <- 0;
if j=q then sol.(i).(j) <- 0;
if t1.(i) = t2.(j) then sol.(i).(j) <- tab.(i+1).(j+1) +1 ;
else sol.(i).(j) <- max tab.(i).(j+1) tab.(i+1).(j);;
done;
done;

 



