let fact n =
  	if n < 0 then raise(Failure "n<0")
  	else
  	        let rec aux = function
  		0 -> 1
		| n -> n*aux(n-1)
  		in aux n 
