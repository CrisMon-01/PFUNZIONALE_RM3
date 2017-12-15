type 'a ntree = Tr of 'a * 'a ntree list

type multi_expr =
MultiInt of int
| MultiVar of string
| MultiDiff of multi_expr * multi_expr
| MultiDiv of multi_expr * multi_expr
| MultiSum of multi_expr list
| MultiMult of multi_expr list

(*multi_expr -> multi_expr -> bool*)
(*sub_expr e1 e2 rit true se e2 è sottoespressione di e1 sub_expr (5+8) 8 = true*)
let rec sub_expr e1 e2 = 
	e1 = e2 || match e1 with
	MultiDiff(x ,y)| MultiDiv(x,y) -> sub_expr x e2 || sub_expr y e2
	|MultiSum l | MultiMult l -> List.exists(function x -> sub_expr x e2) l
	| _ -> false

(*subst multi_expr -> string -> multi_expr-> multi_expr*)
(*data un’espressione E, il nome di una variabile x e un’espressione E 0 , costruisca l’espressione che si ottiene da E sostituendo con E 0 ogni occorrenza della variabile di nome x*)
let rec subst e1 x e2 = match e1 with
	MultiVar y -> if x = y then e2
					else e1
	| MultiInt n -> MultiInt n 
	|MultiDiff (y,z)-> MultiDiff(subst y x e2, subst z x e2)
	|MultiDiv (y,z)-> MultiDiv(subst y x e2, subst z x e2) 
	|MultiSum l -> MultiSum (List.map( function z -> subst z x e2) l)
	|MultiMult l -> MultiMult (List.map( function z -> subst z x e2) l)

(*’a ntree -> ’a list*)
(*dato un albero visita in postordine prima i figli poi la radice in una lista*)
let rec postorder (Tr(x, tlist)) = 
	 (postorderlist tlist)@[x]
and postorderlist = function
	[]->[]
	|t::rest-> (postorder t)@(postorderlist rest)
(*’a ntree -> ’a list*)
(*inorder dato un albero n-ario ritorna una lista con primo figlio radice e lista nodi*)
let rec inorder (Tr(x,tlist)) =
	if tlist = [] then [x]
else (inorder(List.hd(tlist)))@[x]@(inorderlist (List.tl(tlist)))
and inorderlist = function 
	[]->[]
	|t::rest-> (inorder t)@(inorderlist rest)

(*’a list -> ’a ntree -> bool*)
(*controllo che ogni foglia di un albero appartenga alla lista*)
let rec foglie_in_lista lst = function
	Tr(x,[]) -> List.mem(x) lst
	|Tr(x, tlist) -> List.for_all(foglie_in_lista lst) tlist

(*’a ntree -> int*)
(*dato un albero nario restituisce il numero di foglie di un albero n-ario*)
let rec num_foglie = function
	Tr(x,[])->1
	|Tr(x,tlist)-> num_flist tlist
and num_flist = function
	[]-> 0
	|t::rest-> num_foglie t + num_flist rest

(*quaderno*)


let rec coppia_max = function
	[]->failwith "error"
	|[x]-> x
	|(e1,c1)::(e2,c2)::rest -> if(c1>c2) then coppia_max((e1,c1)::rest)
								else coppia_max((e2,c2) :: rest)
(*’int ntree ->(int * int)*)
(*riporto il costo per arrivare alla foglia + distante dalla radice se presente*)
let rec foglia_costo = function
	Tr(x,[])-> 	(x,x)
	|Tr(x, tlist) -> let (a,b) = coppia_max(List.map(foglia_costo )tlist) in (a, x+b)

(*int ntree -> (int * int)list*)
(*riporta una lista di coppi contenente foglia e distanza da radice perogni foglia*)
let rec tutte_foglie_costi = function
	Tr(x,[])->[(x,x)]
	|Tr(x,tlist) -> List.map(function (a,b)->(a, b+x)) (mutua tlist)
and mutua = function
	[]->[]
	|t::rest-> tutte_foglie_costi t @ mutua rest
