let rec permut l2 = function
	[]->true
	|x::rest-> List.mem x l2 && permut l2 rest
let some_all p listona =
List.find (List.for_all p) listona

let somma_all p listona =
List.find(List.for_all p) listona

let rec dim n = function
	[]-> failwith "errore"
	|(a,b)::rest-> if List.mem n a then b 
			else dim n rest

let presucc n l = 
	let rec aux pr n = match l with 
		[]-> 0
		|x::rest-> if x = n then List.length rest
			else aux (p+1) s
	


