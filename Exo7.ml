(*Question1 : on parcourt la lsite et on conte
le nombre d'occurence pour tout i allant de 1
‡ MAX_VAl, puis on remplit le tableau avec le 
nombre d'occurneces correspondnat ‡ chaque nombre.
Enfin pn parcourt le tableau et on repete chaque 
nombre m fois ou m est son occurence*)
(*question 2*)
let tri_simple tab =
let occu = make_vect (MAX_VAl +1) 0 in
let n = tab.(0) in
for i= 1 to n-1 do
  occu.(tab.(i)) <- occu.(tab.(i))+1
  done;
 let k = ref 1 in 
 for j = 0 to Max_Val do
 for i =1 to occu.(j) do
 t.(!k) <- j
 incr k 
 done;
 done;;
 
(*question 3*)
let vider T = T.(0) <-0;;
(*question 4*)
let ajouter T p = 
let n = T.(0) in
T.(0)<-n+1
T.(n) <- p;;
(*question 5*)
let concatener t1 t2 =
let n2 = t2.(0)in
let n1 = t1.(0) in
for i = 1 to (n2-1) do
t1.(n1+i) <- t2.(i)
done;
t1.(0)<-n1+n2;;
(*ou ajouter t1 t2.(i)*)

(*question 6*)
(*complexite = lineaire avec n=t2.(0))

(*question 7*)

let max_valeurs t =
let max = ref -1 in
let n = t.(0) in
for k = 1 ton (n-1) do
if t.(i) > !max then max:= t.(i);
done;!max;;

(*question 8*)
let nombre_chiffres p = 
let rec aux p n = match n with
|o -> n
|n -> aux (p/10) (n+1) in aux p 0;;

(*question 9*)
let max_chiffres T = let m = max_chiffres 
in nombre_chiffres n ;;

(*question 10*)
(* nombre_chiffres est en maxc et
max_chiffres est en t.(0) donc en grand o  de 
n+maxc)

(*question 11*)
(*

Pour r = 2

baquets[0] : 2, 603, 7...
baquets[2] : 3, 20, 423, 27...
baquets[5] : 3, 50, 453, 57, ...

Les autres baquets sont vides. 

La table T est alors : 
8, 603, 7, 20, 423, 27, 50, 453, 57...

Pour r = 3 : 

baquets[0]: 5, 7, 20, 27, 50, 57, ...
baquets[4]: 2, 423, 453...
baquets[6]: 1, 603...

Les autres baquets sont vides 

La table T est alors : 
8, 7, 20, 27, 50, 57, 423, 453, 603, ....
Elle est alors tri√©e. 

*)

(*question 13*)
let distribuer T r baquets = 
let n = T.(0) in 
for i = 1 to (n-1) do
let p = T.(i) in let k = ( p / puissance_10
 (r-1)) mod 10
 ajouter baquets[k] p; done;;
 
 let baquets = make_matrix 10 20 0;
 (*question 14*)
let tri_baquets t = 
let n = t.(0) in 
let maxc = max_valeurs t in
for r = 1 to (maxc -1) do
for i = 1 to 9 do
vider baquets [i] ; done;
distribuer t r baquets
vider t
for i = 1 to 9 do
concatener t baquets[i];done;
done;;