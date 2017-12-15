type expr =
Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr

let rec subexpr sopra sotto =
	if sotto = sopra then true
	else match sopra with
		Sum (x,y) | Diff(x,y) | Mult(x,y) | Div(x,y) -> subexpr x sotto || subexpr y sotto
		| _-> false

let rec subst_in_expr e1 x e2 =
	match e1 with
	Var y-> if y = x then e2 else e1
	| Int k -> Int k
	| Sum (n1,n2) -> Sum(subst_in_expr n1 x e2, subst_in_expr n2 x e2)
	| Mult (n1,n2) -> Mult(subst_in_expr n1 x e2, subst_in_expr n2 x e2) 
	| Diff (n1,n2) -> Diff(subst_in_expr n1 x e2, subst_in_expr n2 x e2) 
	| Div (n1,n2) -> Div(subst_in_expr n1 x e2, subst_in_expr n2 x e2)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec reflect = function
	Empty -> Empty
	|Tr(x,sn,dx)-> Tr(x,reflect dx, reflect sn)

let fullTree n =
    let rec aux i k =
    	if i = 0 then Empty
    	else Tr(k, (aux (i-1) (2*k)), (aux (i-1) (2*k+1)))
    in aux n 1

let rec h = function
	Empty -> 0
	|Tr(_, sn, dx)-> 1 + max (h sn) (h dx) 

let rec balanced = function
	 Empty -> true
	 |Tr( _ , sn , dx ) -> (abs((h sn)-(h dx))) <= 1 && balanced sn && balanced dx

let rec preorder = function
	Empty->[]
	|Tr(x, sn, dx)-> x::(preorder sn@preorder dx)
let rec inorder = function
	Empty->[]
	|Tr(x, sn, dx)-> inorder sn@[x]@inorder dx
let rec postorder = function
	Empty->[]
	|Tr(x, sn, dx)-> (postorder sn@postorder dx)@[x]

let rec take n = function
	[] -> []
	|x::rest-> if (n>0) then x::take (n-1) rest
				else []

let rec drop m = function
	[]-> []
	|x::rest-> if (m>0) then drop (m-1) rest
				else x::rest

let rec balpreorder = function
	[]-> Empty
	| x :: rest-> let l = List.length rest in
					Tr(x, balpreorder (take (l/2) rest), balpreorder(drop(l/2) rest))

let rec balinorder = function
	[]->Empty
	|x -> let l = (List.length x)/2 in
		Tr(List.hd (drop l x), balinorder (take l x), balinorder (List.tl(drop l x)))

let rec foglie_in_lista lst= function
	Empty->true
	|Tr(x,Empty,Empty)-> List.mem x lst 
	|Tr(x,sn,dx)->foglie_in_lista lst sn && foglie_in_lista lst dx

let rec numFoglie = function
	Empty->0
	|Tr(x, Empty,Empty)->1
	|Tr(x,sn,dx)-> numFoglie sn + numFoglie dx

let rec segui_bool lst = function
	Empty->failwith "error"
	|Tr(x,sn,dx) -> if (List.length lst = 0) then x
					else if (List.hd lst) then segui_bool (List.tl lst) sn
							else segui_bool (List.tl lst) dx

let rec foglia_costo = function
	Empty->failwith "error"
	|Tr(x,Empty,Empty)-> (x,x)
	|Tr(x,Empty,dx) -> let(a,b) = foglia_costo dx in (a,x+b)
	|Tr(x,sn,Empty) -> let(a,b) = foglia_costo sn in (a,x+b)
	|Tr(x,sn,dx) -> let (a,b) = foglia_costo dx in let (c,d) = foglia_costo sn in 
					if(b>d) then (a,b+x)
					else (c,d+x)

type expr =
Jolly
| Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr

let rec pattern_Match e1 e2 =
	match (e1, e2) with
	|(_, Jolly) -> true
	|Sum(e11,e12) , Sum(e21,e22) |Diff(e11,e12) , Diff(e21,e22) |Div(e11,e12) , Div(e21,e22) |Mult(e11,e12) , Mult(e21,e22)->
		pattern_Match e11 e21 && pattern_Match e12 e22
	|(ea , eb) -> ea = eb

let rec max_common t1 t2 =
	match (t1,t2) with
	(Empty,Empty)-> Empty
	|(Empty, Tr(_,_,_))
	|(Tr(_,_,_),Empty)-> Tr("@",Empty,Empty)
	|(Tr(a,ts1,td1), Tr(b,ts2,td2))-> if a <> b then Tr("@",Empty,Empty)
										else Tr(a, max_common ts1 ts2,max_common td1 td2)

let rec stessa_str t1 t2 = match (t1,t2) with
	(Empty,Empty)-> true
	|(Tr(_,_,_),Empty)|(Empty,Tr(_,_,_))-> false
	|(Tr(_,ts1,td1),Tr(b,ts2,td2))-> stessa_str ts1 ts2 && stessa_str td1 td2



let rec path_no p = function
	Empty-> failwith "err"
	|Tr(x,Empty,Empty)-> if p x then failwith "err"
							else [x]
	|Tr(x,t1,t2)-> if p x then failwith "err"
					else (try path_no p t1 
							with Failure "err" -> path_no p t2) 

let rec path_corrente lst = function
	Empty-> failwith "err"
	|Tr(x,Empty,Empty)-> if List.mem x lst then [x]
						else failwith "err"
	|Tr(x, sn, dx)-> if List.mem x lst then x::(try path_corrente lst sn 
						with Failure "err" -> path_corrente lst dx)
						else failwith "err" 	

let rec remove x = function
	[] -> failwith "err" 
	|y::rest-> if y = x then rest
				else y::remove x rest

let leaf x = Tr(x,Empty,Empty)

let ti = Tr(0,Tr(10,leaf 2 , leaf 5),
     Tr(6,Tr(6,leaf 3,Empty),
        Tr(4,leaf 3,leaf 4)))

let rec path_coprente lst = function
	Empty -> failwith "err"
	|Tr(x,Empty,Empty) -> if lst = [] || lst = [x] then [x]
							else failwith "err"
	|Tr(x, sn, dx) ->  if List.mem x lst then try x::(path_coprente (remove x lst) sn)
							with _ -> x::(path_coprente (remove x lst) dx)
						else try x::path_coprente lst sn
							 with _ -> x::(path_coprente lst dx) 

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
let lst = [(Rosso,[1;2;4;7;10]); (Giallo,[3;8;11]);(Verde,[0;5;6;13]); (Blu,[9;12;14;15])]

let rec colore a = function
	[]-> failwith "err"
	|(c,l) :: rest-> if List.mem a l then c 
				else colore a rest

let t = Tr(1,
	Tr(2, leaf 8,leaf 3),
	Tr(5,Tr(8,leaf 10,leaf 9),Tr(7,leaf 4, leaf 8)))

let rec path_colorato n lc = function
	|Empty->failwith "err"
	|Tr(x,Empty,Empty) -> if x = n then [x]
							else failwith "err"
	|Tr(x, (Tr(y,_,_) as sn),( Tr(z,_,_) as dx) ) -> let attuale = (colore x lc) in 
			try( if (colore y lc) <> attuale then x::(path_colorato n lc sn)
					else failwith "err")
			with  Failure "err"-> if (colore z lc) <> attuale then x::(path_colorato n lc dx)
									else failwith "err"

let abr= Tr((9,9),
        Tr((5,5),leaf (1,1),leaf (7,7)),Empty)

let rec chekka p = function
	Empty -> true
	|Tr((a,_),sn,dx)-> p a && chekka p sn && chekka p dx

let rec abr_check = function
	Empty-> true
	|Tr((a,b),sn,dx)-> chekka (( > ) a) sn && chekka (( < ) a) dx && abr_check sn && abr_check dx

let rec abr_search x = function
	Empty-> failwith "no_Match"
	|Tr((a,b), sn, dx)-> if x = a then b 
						else if x>a then abr_search x dx
							else abr_search x sn

let rec abr_update (a,b) = function
	Empty->Tr((a,b),Empty,Empty)
	|Tr((x,y), sn , dx) -> if x = a then (Tr((x,b), sn, dx))
							else if x > a then Tr((x,y),abr_update (a,b) sn,dx)
									else Tr((x,y), sn,abr_update (a,b) dx)

let rec abr_delmin = function
	Empty-> failwith "err"
	|Tr(x,Empty,dx)-> (x , dx)
	|Tr(x,sn,dx)->let (a,b) = abr_delmin sn in(a ,Tr(x,b,dx))

let rec delete r = function
	Empty-> Empty
	|Tr((k,v),Empty,Empty) as f ->if k = r then Empty
							else f
	|Tr((k,v),sn,dx)->if k>r then Tr((k,v),delete r sn, dx)
							else if k<r then Tr((k,v),sn,delete r dx) 
									else let(a,b) = abr_delmin dx in Tr(a,sn,b)

let rec abr_insert t x = match t with  
	Empty -> Tr(x,Empty,Empty)
	|Tr(a,sn,dx)->if a >= x then Tr(a,(abr_insert sn x), dx)
					else Tr(a,sn,(abr_insert dx x))

let rec crealb t = function
	[]->t
	|x::rest-> abr_insert (crealb t rest) x 

let rec treesort l = inorder (crealb Empty l)
