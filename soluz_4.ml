let rec length = function
    [] -> 0
  | _::rest -> 1+length rest

let rec sum_of = function
	[] -> 0
	| x::rest -> x + sum_of rest

let rec maxlist = function
	[] -> failwith "listavuota"
	| [x] -> x
	| x :: rest -> max x (maxlist rest)

let maxl  l=
let rec aux tmp = function 
	[]-> tmp
	| x::rest -> if x > tmp
			then aux x rest
			else aux tmp rest
	in match l with
	[] -> failwith "lista vuota"
	| x::rest -> aux x rest

let rec drop n = function
 	[] -> []
	| _::rest -> if n=0 then rest
			else drop (n-1) rest
let rec append lst1 lst2 = match lst1 with
	[] -> lst2
	| x::rest -> append rest lst2
let reverse l =
let rec aux rev = function
	[] -> []
	| x::rest -> aux (x::rev) rest
in aux [] l
let rec nth n = function
 	[] -> failwith "fuoriscala"
	| x::rest -> if n = 0 then x
			else nth (n-1) rest
let remove z l = 
let rec aux y = function
	[] -> []
	| x::rest -> if x = y then aux y rest
			else x :: (aux y rest)
in aux z l

let rec copy n k = match n with
	0 -> []
	| _ -> if n <0 then failwith "copia negative volte"
		else k :: copy (n-1) k
let rec nondec = function
	[]-> true
	|[x]-> true
	|x::y::rest -> x<y && nondec (y::rest)

let rec pairwith z = function
    [] -> []
  | x::rest -> (z,x)::pairwith z rest
let rec duplica = function
	[]->[]
	| x::rest -> x::x::duplica rest
let enumera list =
 let rec aux index = function 
	[]->[]
	| x::rest -> (index,x):: aux (index+1) rest
 in aux 0 list

let rec position a = function
	[] -> failwith "non presente"
	| x::rest -> if x = a then 0
			else 1 + position a rest
let rec alternate lst =
let rec aux index = function
	[] -> []
	|x::rest -> if (index mod 2 = 1) then x:: aux(index+1) rest
			else aux (index+1) rest
	in aux 0 lst
let rec min_dei_max = function
    [] -> failwith "min_dei_max"
  | [lst] -> maxl lst
  | lst::rest ->
        min (maxl lst) (min_dei_max rest)

