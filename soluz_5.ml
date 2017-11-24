let rec combine prima seconda =
  match (prima,seconda) with
    ([],[]) -> []
  | (x::restx,y::resty) -> (x,y)::combine restx resty
  | _ -> failwith "fallimento"

let rec split = function
    [] -> ([],[])
  | (x,y)::rest ->
      let (restx,resty) = split rest
      in (x::restx,y::resty)

let rec cancella a = function
	[] -> []
	|(k,v)::rest -> if k = a then cancella a rest
			else (k,v)::cancella a rest
let rec subset set2 = function
	[] -> true
	| x::rest -> List.exists x set2 && subset set2 rest

let explode s =
let rec aux n str =
	if n > (String.length s)-1 then []
	else s.[n]::aux (n+1) str
in aux 0 s 

let rec implode = function
	[]->""
	|c::rest->(String.make 1 c) ^ implode rest

let rec upto n =
  if n <= 0 then []
  else n::upto(n-1)

let rec pairwith y = function
    [] -> []
  | x::rest -> (y,x)::pairwith y rest
	
let intpairs n =
  let range = upto n in
  let rec aux = function
     [] -> []
   | x::rest ->
        (pairwith x range)@aux rest
  in aux range
let rec trips = function
	x::y::z::rest -> x::y::z::trips(y::z::rest)
	| _-> []
let rec take n = function
	[]-> []
	|x::rest -> if n = 0 then []
			else x::take (n-1) rest
let rec choose n l = match l with
	[]->[]
	|x::rest-> if List.length l >= n then (take n l)::choose (n) rest
			else []  


