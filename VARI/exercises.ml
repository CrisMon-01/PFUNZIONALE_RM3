let rec takewhile p = function
	[]->[]
	|x::rest-> if p x then x::takewhile p rest
			else []
(*partition (a'->bool)-> a'list->(a'list*a'list)*)
(*partition mette primo elemento chi soddisfa secondo chi non soddisfa*)
let rec partition p = function	
	[]->([],[])
	|x::rest-> let(yes,no)=partition p rest in if p x then (x::yes,no)
					else (yes, x::no)
(*a'list->a'list->a'list*)
(*rit differenza insiemistica tra 2 liste*)
let setdiff l1 l2 =
	List.filter(fun x ->not (List.mem x l2))l1

