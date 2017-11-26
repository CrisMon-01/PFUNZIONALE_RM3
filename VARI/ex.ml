let somma_ore (h1,m1) (h2,m2)=
	if (h1>=0 && h1<24 && h2>=0 && h2<24 && m1>=0 && m1<60 && m2>=0 && m2<60)
		then ((h1+h2+(m1+m2)/60)mod 24, (m1+m2)mod 60)
	else failwith "error"

let rec read_max () =
try let n = read_int()
in try max n (read_max())
	with _-> n
with _ -> failwith "error"

let rec read_min () =
	try let n = read_int()
	in try min n (read_min())
		with _ -> n
	with _ -> failwith "errore"

let read_max_min ()=
let rec aux nmin nmax =
	try let att = read_int()
		in aux (min att nmin) (max att nmax)
	with _ -> (nmin,nmax)
in try let n = read_int()
	in aux n n 
with _ -> failwith "seq vuota"

let rec tutti_min_di n =
	try let att = read_int()
		in n < att && tutti_min_di n 
	with _ -> true

let rec occorre n = 
	try let att = read_int()
		in n = att || occorre n 
	with _ -> false

let nstr () =
	let rec aux n = 
	match read_line() with
	"" -> n 
	| _ -> aux (n+1)
	in aux 0 

let stringa_max () =
	let rec aux s = 
		let ss = read_line()
		in match ss with
		"" -> s
		| _ -> if String.length s > String.length ss 
				then aux s 
				else aux ss
	in aux ""
		
let rec sumbetwn n m =
  if m>=n then n+sumbetwn (n+1) m
  else n  

let rec length = function
	[]-> 0
	|x::rest-> 1 + length rest

let rec sumofint = function
	[]-> 0
	|x::rest-> x+sumofint rest

let maxof list = 
	let rec aux a = function
		[]-> a
		| x:: rest-> if x>a then aux x rest
						else aux a rest
	in try aux (List.hd list) list
		with _ -> failwith "no lista"

let rec drop n = function
	[]-> []
	|x::rest -> if n = 0 then rest
				else drop (n-1) rest

let  append l1 l2 = 
	let rec aux l2 = function
	[]->l2
	|x::rest-> x::(aux l2 rest)
	in aux l2 l1 

let rec reverse = function
	[]-> []
	|x::rest-> (reverse rest)@ [x]

let rec copy n a =
	match n with 
	0-> []
	| _ -> a::(copy (n-1) a)

let rec nondec = function
	[]->true
	|[x]-> true
	|x::y::rest-> (x<y) && nondec (y::rest) 

let rec pairwith a = function
[]->[]
|x::rest -> (a,x)::pairwith a rest

let rec duplica = function
[]-> []
|x::rest -> x::x::duplica rest


let rec alterna = function
	[]->[]
	|[x]-> []
	|y::z::rest -> y::(alterna rest)	

let enum l =
	let rec aux n = function
	[]-> []
	|x::rest-> (n,x)::(aux (n+1) rest)
in aux 0 l

let rec lista_max = function
	[]->[]
	|l::rest->(maxof l) :: lista_max rest

let max_of_min ll = 
	let lista_m = (lista_max ll) in 
	let rec aux = function
	[]-> 0
	|[x]->x
	|x::rest-> min x (aux rest)
	in aux lista_m

let rec split = function
[]->([],[])
|[x]-> ([x],[])
|x::y::rest-> let (a,b) = split rest in (x::a,y::b)

let rec search x = function
	[]-> true
	|y::rest-> x=y || search x rest

let rec subset s2 = function
	[]-> true
	|x::rest-> search x s2 && subset s2 rest
	
let rec implode = function
	[]-> ""
	|x::rest-> (String.make 1 x)^implode rest

let explode s = 
	let rec aux n ss= 
	if n < (String.length ss) then ss.[n]::(aux (n+1) ss)
	else []
in aux 0 s

let rec list_of n = 
	if n <= 0 then []
	else n::(list_of (n-1))
let rec inscoppia y = function
	[]->[]
	|x::rest-> (y,x)::inscoppia y rest

let inpairs n = 
	let fino_a = list_of n in
	let rec aux m = function
		[]-> []
		|x::rest-> (m,x)::(aux (m-1) rest)@aux x rest	
	in aux n fino_a

let rec trips = function
	x::y::z::rest-> (x,y,z)::trips(y::z::rest)
	|_ -> []

let rec select n = function
	[]->[]
	|x::rest-> if n > 0 then x::select(n-1) rest
				else []

let rec choose n = function
	[]->[]
	|x::rest-> if((List.length rest)>=(n-1)) then 
						(x::select (n-1) rest)::(choose n rest)
				else []

let rec find p = function
	[] -> failwith "no found"
	|x::rest-> if p x then x 
				else find p rest

let rec takewhile p = function
[]->[]
|x::rest-> if p x then x::takewhile p rest 
			else []
let rec  dropwhile p = function
	[]->[]
	|x::rest-> if p x then dropwhile p rest 
				else x:: rest

let rec partition p = function
	[]->([],[])
	|x::rest-> let(yes,no) = partition p rest in if p x then (x::yes, no)
											else (yes,x::no)

let rec pairwith y = function
	[]->[]
	|x::rest-> (y,x)::pairwith y rest

let verificamatrice n ll =
	List.exists(List.for_all(function x-> x<n) ) ll

let setdiff l1 l2 =
	List.filter(function x -> not(List.mem x l1)) l2

let subset s1 s2 =
	List.for_all(function x-> List.mem x s1)s2

let duplica list = 
	List.map(function x -> x+x) list

let mapcons b l =
	List.map(function x -> (fst x, b::(snd x)))l

let rec tutte_list n x y =
	if n = 0 then [[]]
	else let l = tutte_list (n-1) x y in
		(List.map(List.cons x )l)@(List.map(List.cons y) l)

let rec interleave a = function
	[]->[[a]]
	|x::rest->(a::x::rest)::(List.map(List.cons x )(interleave a rest))

let rec find x = function
    [] ->([],[])
  | y::rest -> 
  		let (a,b) = find x rest in
  		if x = y then ([], rest)
  		else (y::a,b)

let rec spezza y = function
	[]-> ([],[])
	|x::rest-> if y=x then find y rest
				else spezza y rest