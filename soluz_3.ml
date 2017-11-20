let rec sumto n =
if n > 0 then n + sumto (n-1)
else 0

let verifica h m =
	h>=0 && h <24 && m>=0 && m<60
let somma_ore (h1,m1) (h2,m2) =
if verifica h1 m1 && verifica h2 m2
	then	if((m1+m2)>59) then ((h1+h2+1)mod 24 , (m1+m2) mod 60)
		else ((h1+h2) mod 24 , m1+m2)
else failwith "errore"

let read_max () =
  let rec aux n =
    try 
      let k = read_int()
      in aux (max n k)
    with _ -> n
  in try aux (read_int())
     with _ -> failwith "Sequenza vuota" 

let rec read_max_r () =
try let n = read_int()
in try max n (read_max_r())
	with _-> n
with _ -> failwith "error"

let rec read_min_r ()=
try let k = read_int()
in try min k (read_min_r())
with _-> k
with _ -> failwith"error"

let tutti_min() =
try let min = read_int()
in let rec aux m=
	try if(min<read_int()) then aux m
	else false
	with _ -> true
in aux min
with _ -> failwith "errore"


let rec occorrenze n =
try let k = read_int()
in occorrenze n || n = k	
with _ -> false

let rec sumbetween n m =
if n >= m then 0
else sumbetween (n+1) m  + n

let num_string () =
let rec aux c =
if read_line() = "" then c
else 1 + aux c
in aux 0

let string_max () =
let rec aux l =
let attual = read_line()
in if attual = "" then l
    else if  String.length attual > String.length l then aux attual
	else aux l 
in aux ""

let rec power n k =
if k = 0 then 1
else n * power n (k-1)	

let rec fib  = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2)


let max string =
   let rec aux (string, i) =
     if i=(String.length string) then string.[i-1]
      else aux(string, (i+1))
   in aux (string,0) 


