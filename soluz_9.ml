type 'a ntree = Tr of 'a * 'a ntree list
let leaf n = Tr(n,[])
let t = Tr(1,[Tr(2,[Tr(3,[leaf 4;
                          leaf 5]);
                    Tr(6,[leaf 7]);
                    leaf 8]);
              leaf 9;
              Tr(10,[Tr(11,[leaf 12;
                            leaf 13;
                            leaf 14]);
                     leaf 15;
                     Tr(16,[leaf 17;
                            Tr(18,[leaf 19;
                                   leaf 20])])])])
let t1 = Tr(9,[Tr(2,[Tr(3,[leaf 4;
                          leaf 5]);
                    Tr(6,[leaf 7]);
                    leaf 8]);
              leaf 9;
              Tr(13,[Tr(10,[leaf 12;
                            leaf 13;
                            leaf 14]);
                     leaf 16;
                     Tr(16,[leaf 17;
                            Tr(18,[leaf 19;
                                   leaf 20])])])])

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

(*int ntree -> (int * int)list￼*)
(*riporta una lista di coppi contenente foglia e distanza da radice perogni foglia*)
let rec tutte_foglie_costi = function
	Tr(x,[])->[(x,x)]
	|Tr(x,tlist) -> List.map(function (a,b)->(a, b+x)) (mutua tlist)
and mutua = function
	[]->[]
	|t::rest-> tutte_foglie_costi t @ mutua rest

let rec cancella x = function
	[] -> failwith "err"
	|y::rest-> if x = y then rest
				else y::cancella x rest
(*'a ntree -> 'a list -> 'a ->'a list*)
(*dato un albero e una lista e k riporti se esiste un percorso rad->foglia:k passando una volta x i nodi contentui in l*)
let rec ramo_da_lista t l a = match t with
	Tr(x,[]) -> if(x = a && l = [a]) then [x]
					else failwith "err"
	|Tr(x,tl)-> if List.mem x l then x::(rdl_l a (cancella x l) tl)
				else failwith "err"
and rdl_l k lst = function
	[]->failwith "err"
	|t::ts -> try ramo_da_lista t lst k 
				with _-> rdl_l k lst ts 
				
let primo k = 
	 let rec aux n =
	 if k = n then true else if k <= 1 then true
		else if (k mod n) = 0 then false
		  	else true && aux (n+1)
		in aux 2 

let rec ramo_di_primi =function
	Tr(x,[]) -> if primo x then x
				else failwith "err"
	|Tr(x,tl) -> if primo x then r_p_l tl
					else failwith "err"
and r_p_l = function
	[]->failwith "err"
	|t::rest-> try ramo_di_primi t 
				with _ ->r_p_l rest	

let rec path_non_pred p t = match t with 
	Tr(x,[])-> if p x then failwith "err"
				else [x]
	|Tr(x, tl)-> if p x then failwith "err"
					else x::(pnp_l p tl)
and pnp_l p = function
	[]-> failwith "err"
	|t::ts -> try path_non_pred p t 
				with _ -> pnp_l p ts

let rec same_str t1 t2 = match (t1,t2) with
	|(Tr(x,[]),Tr(y,[]))->true
	|(Tr(x,tl1),Tr(y,tl2))-> (List.length tl1 = List.length tl2) && s_st_l tl1 tl2
	|(_,_)-> false
and s_st_l l1 l2 = match (l1,l2) with
	([],[])-> true
	|(a::r1, b::r2)-> same_str a b && s_st_l r1 r2 
	|(_,_)-> false

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
let lst = [(Rosso,[1;2;4;7;10]); (Giallo,[3;8;11]);
(Verde,[0;5;6;13]); (Blu,[9;12;14;15])]

let rec radici = function
	[]->[]
	|Tr(x,_)::rest-> x::radici rest

let rec colore n = function	
	[]->failwith "err"
	|(a,b)::rest-> if List.mem n b then a 
					else colore n rest


let rec ramo_colorato a lst t = match t with 
	Tr(x,[])-> if x = a then [x]
				else failwith "err"
	|Tr(x,tl)-> let attuale = colore x lst in 
				x::(r_c_list a lst (List.filter (function y-> (colore (root y) lst) <> attuale) tl ))
and r_c_list a lst = function
	[]-> failwith "err"
	|tr::rest-> try ramo_colorato a lst tr 
				with _-> r_c_list a lst rest	

