let rec find p = function
	[]-> failwith "noMatch"
	|x::rest-> if p x then x
			else find p rest

let rec takewhile p = function
	[]-> failwith "noMatch"
	|x::rest->	if p x then [x]@(takewhile p rest)
			else []

let rec dropwhile p = function
	[]-> []
	| x::rest -> if p x then dropwhile p rest
			else x::rest

let rec partition p = function
    [] -> ([],[])
  | x::rest -> 
      let (yes,no) = partition p rest
      in if p x then (x::yes,no)
      else (yes,x::no)

let rec partitioning p = function
	[]->([],[])
	|x::rest-> if p x then (x::fst (partitioning p rest), snd (partitioning p rest))
			else (fst (partitioning p rest), x::snd (partitioning p rest))

let pairwith y l =
	let coppia x = (y,x) 
	in List.map coppia l
	
let verifica_matrice n mat =
  List.exists  (List.for_all (function x -> x<n)) mat

let verifica_mat n list =
	let p x = x<n
	in List.exists (List.for_all p) list	

let setdiff l1 l2 =
	List.filter(function x -> List.mem x l1) l2
let subset l1 l2 =
	List.for_all(function x -> List.mem x l2) l1
let raddoppia l=
	List.map(function x-> x+x) l
let mapcons lis n=
	List.map(function l -> (fst l,n::snd l)) lis

let rec interleave y= function
	[]->[[y]]
	|x::rest-> (y::x::rest)::(List.map(List.cons x) (interleave y rest))

let rec tutte_liste n x y =
	if n = 0 then [[]]
	else let l = tutte_liste (n-1) x y in (List.map(List.cons x)l)@(List.map(List.cons y)l)

let rec permut = function
	[]->[[]]
	|x::rest->  List.flatten(List.map( fun y -> interleave x y) (permut rest))
 

