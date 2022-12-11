type point = {X:float; Y:float};;

let dist point1 point2 = sqrt((point1.X -. point2.X)*.(point1.X -. point2.X) +. ((point1.Y-.point2.Y)*.(point1.Y-.point2.Y)));;
(*n^2*)
let ordre_x point1 point2 = point1.X<= point2.X;;

let ordre_y point1 point2 = point1.Y<= point2.Y;;


#open "stack"

let selec ty xmin xmax =
let p = new() and n=ref 0 in 
for i=0 to (vect_length(ty))-1 do
if ty.(i).X >= xmin && ty.(i).X <= xmax then
begin push t.(i) p ; incr n end
done;;

let plus_proches_bande tz =
let d=ref distance tz.(1) tz.(0) and cmin = ref (1,0) in
for k= 0 to vect_length(tz)-8 do
for l=1 to 7 do
let a,b = cmin in
if distance tz.(k) tz.(l) < !d then begin d := distance tz.(k) tz.(l); cmin := (tz.(k),tz.(l)) 
done;
done;;




