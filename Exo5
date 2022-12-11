type prop=
|Vrai
|Faux
|X of int
|Non of prop
|Ou of prop*prop
|Et of prop*prop
|Implique of prop*prop
|Equivaut of prop*prop;;

let connecteur_bool = ["non";"ou";"et";"=>";"<=>";"vrai";"faux"];;

let test =["P";"non";"P";"Q";"ou";"et";"R";"<=>"];;

let var_libres li =
let rec aux li accu = match li with
|[]-> accu
|x::q when mem x connecteur_bool -> aux q accu
|x::q when mem x accu  -> aux q accu 
|x::q -> aux q (x::accu) in aux li [];;

let num li x = 
let rec aux li x n = match li with 
|[] -> failwith "non present"
|y::q when y=x -> n+1
|y::q -> aux q x (n+1) in aux li x 0;;
#open "stack";;

let convertit li = 
let p = new () and var=var_libres li in
let rec remplit li p = match li with
|[] -> ()
|x::q when x= "vrai"-> push p Vrai; remplit q p
|x::q when x="faux" -> push p Faux; remplit q p
|x::q when x="non"  -> let x = pop p in push p Non(x) ; remplit q p
|x::q when x= "ou"  -> let x = pop p in let y= pop p in push p (Et(x,y)) ; remplit q p
|x::q when x= "et " -> let x = pop p in let y= pop p in push p (Ou(x,y)) ; remplit q p
|x::q when x= "=>"  -> let x = pop p in let y= pop p in push p (Implique(y,x); remplit q p
|x::q when x= "<=>" -> let x = pop p in let y= pop p in push p (Equivaut(x,y)); remplit q p
|x::q  -> let n = num x var in push p (X(n)); remplit q p in

let rec eval  env fl = match fl with
|Vrai -> true
|Faux ->  false
|X n -> env.(n)
|Non (fl1) -> not ( eval env fl1)
|Ou (fl1 ,fl2) -> eval env fl1 || eval env fl2
|Et (fl1,fl2) -> eval env fl1 && eval env fl2
|Implique (fl1, fl2) -> not(eval env fl1) || eval env fl2
|Equivaut (fl1, fl2) -> eval env fl1 = eval env fl2;;



begin
remplit li p; match p.Contenu with:
|x::[] -> [x]
|_-> failwith"Conversion" 
end;;

let incr env = 
let n = vect_length env in
let i = ref (n-1) in
while  !i >0 && env.(!i)=false do
env.(!i) <- false;
decr i
done;
if !i= (-1) then failwith "pas de suivant"
else env.(!i) <-true;;

let rec max_x fl = match fl with
|Vrai -> -1
|Faux ->  -1
|X n -> n
|Non (fl1) -> max_x fl1
|Ou (fl1 ,fl2) -> max(max_x fl1) (max_x fl2)
|Et (fl1,fl2) -> max(max_x fl1) (max_x fl2)
|Implique (fl1, fl2) -> max(max_x fl1) (max_x fl2)
|Equivaut (fl1, fl2) -> max(max_x fl1) (max_x fl2);;

let satisfait fl = 
let n = max_x fl in
let env = make_vect (n+1) false
in while not (eval env fl) do
incr env done;
env;;

let tautologie fl =
let n = max_x fl in
let compteur = ref 0 in
let max = puissance_2 (n+1) in
let env = make_vect (n+1) false
in while not (eval env fl) && compteur < max-1 do
incr env, compteur := !compteur+1 done; 
if !compteur=max-1 then  Tautologie
else Contre_exemple env;;



