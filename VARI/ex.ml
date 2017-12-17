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

let pairwith_map a l =
	List.map(function x-> (a,x))l

let rec tuttle_l n a b =
	if n = 0 then [[]]
	else let ll = tuttle_l (n-1) a b in
		(List.map (function x -> a::x)ll)@(List.map(List.cons b)ll)

let some_all p ll =
	List.find(List.for_all p) ll

let permutazione l1 l2 = 
	List.for_all(function x -> List.mem x l2)l1	

let rec dim x = function
	[]-> failwith "no match"
	|y::rest-> if (List.mem x (fst y))
					then snd y
				else dim x rest

let rec presucc y = function
	[]->failwith "errore"
	|x::rest-> if x = y then (0, List.length rest)
				else let (a,b)=presucc y rest
					in (a+1,b)

let rec sum = function
	[]->(0,0)
	|x::rest-> let (k1,k2) = sum rest 
				in (k1 + fst x,k2 + snd x)

let rec prec_succ_lst y = function
	[]->(0,0)
	|x::rst-> let (a,b)=prec_succ_lst y rst in
				try let (p,s) = presucc y x in (a+p,b+s)
				with _-> (a,b)
	
let purge a ll = 
	List.filter(function x -> not(List.mem a x))ll

let rec splits = function
	[]->([],[])
	|[x]->([x],[])
	|x::y::rest-> let (xs,ys) = splits rest in (x::xs,y::ys)

type 'a ntree = Tr of 'a * 'a ntree list
let leaf n = Tr(n, [])
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

(*sett 16*)
let rec subt a t = match t with 
	|Tr(x, tl) -> if x = a then tl @ subt_l a tl
					else subt_l a tl
and subt_l a = function
	[]-> []
	|t::ts-> subt a t @ subt_l a ts

	(*lug 15*)
	let t =
	Tr(1,[Tr(2,[Tr(5,[]);
	Tr(8,[Tr(9,[Tr(5,[])]);            Tr(10,[])])]);
	Tr(3,[Tr(6,[]);
	Tr(7,[]);
	Tr(8,[Tr(9,[]);            Tr(4,[])])]);
	Tr(4,[Tr(9,[]);
	Tr(10,[]);
	Tr(9,[Tr(2,[])])])]) 

let root (Tr(x,_)) = x 
let rec radici = function
	[]->[]
	|t::ts-> if List.mem (root t) (radici ts) then radici ts 
				else (root t)::(radici ts)

let rec accoppia a = function
	[]->[]
	|x::rst-> (a,x) :: accoppia a rst
let rec union set1 = function
	[]->set1
	|x::rest-> if not(List.mem x set1) then x::union set1 rest
				else union set1 rest

let rec archi = function 
	Tr(x,[]) -> []
	|Tr(x,tl)-> let rad = radici tl in
				union (accoppia x rad) (archi_l tl)
and archi_l = function
	[]->[]
	|t::ts-> union (archi t) (archi_l ts) 

(*feb 15*)
let rec mkset = function
	[]->[]
	|x::rest-> if List.mem x rest then mkset rest
				else x::(mkset rest)

let rec nodi = function
	Tr(x,[]) -> [x]
	|Tr(x,l) -> mkset (x::(nodi_l l))
and nodi_l = function
	[]->[]
	|t::ts -> nodi t@nodi_l ts

let root (Tr(a,_))=a
let rec figli a = function
	Tr(x,l)-> mkset(if x = a then ((List.map(root )l)@(figli_lista a l))
					else (figli_lista a l) )
and figli_lista a = function
	[]->[]
	|t::ts-> (figli a t)@(figli_lista a ts)



(*a' tree->a'list*)
(*dato un albero ritorna i nodi con un n di figli = alla prof*)
let prof2figli t = 
	let rec aux p = function
		Tr(x,ls)-> if List.length ls = p then x::(p2f_lis (p+1) ls)
					else p2f_lis (p+1) ls
	and p2f_lis k = function
		[]->[]
		|tr::ts-> (aux k tr)@(p2f_lis k ts)
	in aux 0 t

type  'a btree = Empty
				 |Tr of 'a * 'a btree * 'a btree
let leaf n = Tr(n,Empty,Empty)
let bin = Tr(1,Tr(2,Empty,Tr(3,leaf 3,leaf 5)),Tr(1,leaf 7,Empty))

(*(a->bool) a' btree-> bool*)
(*dato un albero verifica se tutti i figli sn che verificano un condizione*)
let root a = match a with 
		 Empty-> failwith "Empty"
		 |Tr(x,_,_) -> x

let rec verifs p = function
	Empty -> true	
	|Tr(x,Empty,Empty) -> p x
	|Tr(x,sn,dx) -> try (p (root sn))  && verifs p sn && verifs p dx 
						with _ -> verifs p dx 

let valoreUprof t =
	let rec aux p = function
		Empty -> failwith "err"
		|Tr(x,Empty,Empty) -> if x = p then x 
							else failwith "err"
		|Tr(x,sn,dx) -> if x = p then x 
						else try (min (aux (p+1) sn) (aux (p+1) dx) ) with _-> try (aux (p+1) dx )with _-> aux (p+1) sn 	
	in aux 0 t

type expr =
Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr

let rec subexpr e1 e2 = 
	e1 = e2 ||
	match e1 with
	Sum( x , y) | Mult (x,y) | Diff (x,y) | Div (x,y) -> subexpr x e2 || subexpr y e2
	|_->false	

let rec subs e1 s e2 =
	match e1 with 
		Var y -> if s = y then e2
					else e1
		|Int n -> Int n
		|Sum(a,b)-> Sum(subs a s e2, subs b s e2)
		|Diff(a,b)-> Diff(subs a s e2, subs b s e2)
		|Mult(a,b)-> Mult(subs a s e2, subs b s e2)
		|Div(a,b)-> Div(subs a s e2, subs b s e2)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec reflect = function	
	Empty-> Empty
	|Tr(x,sn,dx)-> Tr(x,reflect dx, reflect sn)

let rec checkfull = function
	Empty -> true
	|Tr(x,Empty,_) | Tr(x,_,Empty)-> false
	|Tr(x,sn,dx) -> checkfull sn && checkfull dx 

let fulltree n =
	let rec aux m h=
		if n >= h then
			Tr(m, aux (2*m) (h+1), aux((2*m)+1) (h+1)) 
		else Empty
		in aux 1 1

let rec h = function
	Empty->0
	|Tr(x,sn,dx)-> 1+ max (h dx) (h sn)

let rec preorder = function
	Empty->[]
	|Tr(x,sn,dx)->x::(preorder sn)@(preorder dx)
let rec postorder = function
	Empty->[]
	|Tr(x,sn,dx)-> (postorder sn)@(postorder dx)@[x]
let rec simmetric = function
	Empty->[]
	|Tr(x,sn,dx)-> (simmetric sn)@[x]@(simmetric dx)

let rec balanced = function
	Empty-> true
	|Tr(x,sn,dx)->(abs((h sn) - (h dx) ))<=1 && balanced sn && balanced dx	

let rec take n = function
	[]->[]
	|x::rest-> if n>0 then x::take(n-1) rest
				else []
let rec dropn n = function
	[]->[]
	|x::rest-> if n>0 then drop (n-1) rest
					else x::rest

let rec balpreorder = function
	[]-> Empty
	| x :: rest-> let l = List.length rest in
					Tr(x, balpreorder (take (l/2) rest), balpreorder(drop(l/2) rest))

let rec balinorder = function
	[]->Empty
	|x -> let l = (List.length x)/2 in
		Tr(List.hd (drop l x), balinorder (take l x), balinorder (List.tl(drop l x)))

let rec foglie_in_lista lst = function
	Empty->true
	|Tr(x,Empty,Empty) -> List.mem x lst
	|Tr(x,sn,dx)-> foglie_in_lista lst sn && foglie_in_lista lst dx 

let rec numF = function
 	Empty->0
 	|Tr(x,Empty,Empty)->1
 	|Tr(x,sn,dx)-> numF sn + numF dx

let rec seguiBool lst = function
	Empty -> failwith "err"
	|Tr(x,sn,dx)-> if List.length lst = 0 then x
					else if List.hd lst then seguiBool (List.tl lst) sn
						else seguiBool (List.tl lst) dx

let rec foglia_costo = function
	Empty -> failwith "err"
	|Tr(x,Empty,Empty)-> (x,x)
	|Tr(x,dx,sn)-> try let (a,b) = foglia_costo dx in let (c,d) = foglia_costo sn
			in if (b>d) then (a,b+x) 
				else (c,d+x)
			with _-> try let (a,b) = foglia_costo dx in (a,x+b)
					with _-> let (a,b) = foglia_costo sn in (a,x+b)

let rec stessa_str t1 t2 = match (t1,t2) with 
 	(Empty,Empty)-> true
 	|(Tr(_,_,_),Empty)|(Empty,Tr(_,_,_))->false
 	|(Tr(_,ts1,td1),Tr(_,ts2,td2))-> stessa_str ts1 ts2 && stessa_str td1 td2

let rec max_common t1 t2 = match (t1,t2) with
	(Empty,Empty)-> Empty
	|(Empty,Tr(_,_,_))|(Tr(_,_,_),Empty)-> Tr("@",Empty,Empty)
	|(Tr(a,ts1,td1),Tr(b,ts2,td2))-> if a<>b then Tr("@",Empty,Empty)
									else Tr(a,max_common ts1 ts2,max_common td1 td2)

let rec path_no p = function
	Empty-> failwith "err"
	|Tr(x,Empty,Empty)-> if p x then failwith "err"
							else [x]
	|Tr(x,sn,dx)-> if p x then failwith"err"
					else(try path_no p sn
					with _ -> path_no p dx)

let rec path_corrente lst = function
	Empty-> failwith "err"
	|Tr(x,Empty,Empty)-> if List.mem x lst then [x]
							else failwith "err"
	|Tr(x,sn,dx)-> if List.mem x lst then x::(try path_corrente lst sn
									with _ -> path_corrente lst dx)
					else failwith "err"

let rec remove x = function
	[]->failwith "err"
	|y::rest-> if y = x then rest
					else y ::(remove x rest)

let rec path_coprente lst = function
	Empty -> failwith "err"
	|Tr(x,Empty,Empty) ->  if lst = [x] then [x]
							else failwith "err"
	|Tr(x,sn,dx) ->	if List.mem x lst then try(x::path_coprente (remove x lst) sn)
				with _ -> x::(path_coprente (remove x lst) dx)
				else try x::(path_coprente lst sn)
						with _ -> x::(path_coprente lst dx) 

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
let lst = [(Rosso,[1;2;4;7;10]); (Giallo,[3;8;11]);(Verde,[0;5;6;13]); (Blu,[9;12;14;15])]

let rec colore a = function
	[]-> failwith "err"
	|(c,l)::rest-> if List.mem a l then c		
					else colore a rest	 

let rec path_colorato n lc = function
	|Empty-> failwith "err"
	|Tr(x,Empty,Empty)-> if x = n then [x]
						else failwith "err"
	|Tr(x,(Tr(y,_,_) as sn),(Tr(z,_,_) as dx)) -> let attuale = (colore x lc) in
						try (if (colore y lc) <> attuale then x::(path_colorato n lc sn)
								else failwith "err")
						with _ -> if (colore z lc) <> attuale then x::(path_colorato n lc dx)
									else failwith "err"

let rec check p = function
	Empty-> true
	|Tr((a,b),sn,dx)-> p a && check p sn && check p dx

let rec abr_check = function
	Empty -> true
	|Tr((a,b),sn,dx)-> check (function x -> x < a) sn && check (function x-> x<a) dx && abr_check dx && abr_check sn

let rec abr_search x = function
	Empty-> failwith "err"
	|Tr((a,b),sn,dx)-> if x = a then b
						else(if(x>a) then abr_search x dx
							else abr_search x sn)

let rec abr_update (a,b) = function
	Empty-> Tr((a,b),Empty,Empty)
	|Tr((x,y),sn,dx)-> if x = a then Tr((x,b),sn,dx)		
						else if x > a then Tr((x,y),abr_update (a,b) sn,dx)
									else Tr((x,y),sn,abr_update(a,b) dx)

let rec abr_delmin = function
	Empty-> failwith "err"
	|Tr(x,Empty,dx)->(x,dx)
	|Tr(x,sn,dx)-> let (a,b) = abr_delmin sn in (a,Tr(x,b,dx))

let rec delete r = function
	Empty-> Empty
	|Tr((k,v), Empty,Empty) as f -> if k = r then Empty
							else f
	|Tr((k,v),sn,dx)-> if k > r then Tr((k,v),delete r sn,dx)
						else if k < r then Tr((k,v),sn,delete r dx)
							else let (a,b) = abr_delmin dx in Tr(a,sn,b)

let rec abr_insert t x = match t with 
	Empty-> Tr(x,Empty,Empty)
	|Tr(a,sn,dx)-> if a >= x then Tr(a,(abr_insert sn x),dx)
					else Tr(a,sn,abr_insert dx x)

let rec inorder = function
	Empty->[]
	|Tr(x, sn, dx)-> inorder sn@[x]@inorder dx

let rec crealb_dalist_ordin t = function
	[]-> t
	|x::rest-> abr_insert (crealb_dalist_ordin t rest) x

let rec treesort l = inorder (crealb_dalist_ordin Empty l)

(*)						
let rec abr_update (a,b) = function
	Empty-> Tr((a,b),Empty,Empty)
*)
type 'a ntree = Tr of 'a * 'a ntree list 
let rec pro_nario k = function
	Tr(x,[])-> if k = x then 0
				else failwith "err"
	|Tr(x,lst)-> if x = k then 0
					else 1+(pro_l k lst)
and pro_l k = function
	[]-> failwith "err"
	|t::ts-> try pro_nario k t with _ -> pro_l k ts

let max_p_bin k t = 
	let rec aux pr k = function
		Empty-> failwith "err"
		|Tr(x,sn,dx)-> if x = k then  pr
			else let n =
				max (try aux (pr+1) k sn with _ -> -1) (try aux (pr+1) k dx with _ -> -1)
				in if n = -1 then failwith "err"
					else n
	in aux 0 k t

let rec max_lista = function
	[x]->x
	|x::rest -> max x (max_lista rest)

type multi_expr =
MultiInt of int
| MultiVar of string
| MultiDiff of multi_expr * multi_expr
| MultiDiv of multi_expr * multi_expr
| MultiSum of multi_expr list
| MultiMult of multi_expr list

let rec sub_expr e1 e2 = 
	e1 = e2 || match e1 with
	MultiDiff(x ,y)| MultiDiv(x,y) -> sub_expr x e2 || sub_expr y e2
	|MultiSum l | MultiMult l -> List.exists(function x -> sub_expr x e2) l
	| _ -> false

let rec subst e1 x e2 = match e1 with
	MultiVar y -> if x = y then e2 
					else e1
	|MultiInt n -> MultiInt n
	|MultiDiff (y,z) -> MultiDiff(subst y x e2 ,subst z x e2)
	|MultiDiv (y,z)-> MultiDiv(subst y x e2, subst z x e2) 
	|MultiSum l -> MultiSum (List.map( function z -> subst z x e2) l)
	|MultiMult l -> MultiMult (List.map( function z -> subst z x e2) l)

let rec postorder (Tr(x, tlist)) = 
	 (postorderlist tlist)@[x]
and postorderlist = function
	[]->[]
	|t::rest-> (postorder t)@(postorderlist rest)

let rec inorder (Tr(x,tl))=
	inorder(List.hd(tl))@[x]@(inorder_ls(List.tl tl))
and inorder_ls = function
	[]->[]
	|t::ts->(inorder t)@(inorder_ls ts)

let rec foglie_lista ls = function
	Tr(x,[])->List.mem x ls  
	|Tr(x,tl)-> List.for_all(foglie_lista ls) tl

let rec num_fo = function
	Tr(x,[])->1
	|Tr(x,tl)-> num_f_ls tl
and num_f_ls = function
	[]-> 0
	|t::ts -> num_fo t + num_f_ls ts

let rec coppia_max = function
	[]->failwith "error"
	|[x]-> x
	|(e1,c1)::(e2,c2)::rest -> if(c1>c2) then coppia_max((e1,c1)::rest)
								else coppia_max((e2,c2) :: rest)

let rec foglia_costo = function
	Tr(x,[])->(x,x)
	|Tr(x,tl)-> let (a,b) = coppia_max(List.map foglia_costo tl) in (a,b+x)

let rec cancella x = function
	[]-> failwith "err"
	|y::rst-> if x = y then rst
				else y :: cancella x rst

let rec ramo_da_lista t l a = match t with 
	Tr(x,[])-> if (x = a && l = [a]) then [x]
				else failwith "err"
	|Tr(x,tl)-> if List.mem x l then x::(rdl_l a (cancella x l) tl)
					else failwith "err"
and rdl_l a l = function
	[]-> failwith"err"
	|t::ts -> try ramo_da_lista t l a 
			with _ -> rdl_l a l ts

let primo k = let rec aux n =
		if k = n then true else if k<=1 then true
			else if (k mod n) = 0 then false
		else true && aux (n+1)
	in aux 2

let rec ramo_primi = function
	Tr(x,[])-> if primo x then [x]
				else failwith "err"
	|Tr(x,tl)-> if primo x then r_p_l tl
				else failwith "err"
and r_p_l = function
	[]->failwith "err"
	|t::ts-> try ramo_primi t with _-> r_p_l ts 

let rec path_no_pred p = function
	Tr(x,[])-> 	if p x then failwith "err"
				else [x]
	|Tr(x,tl)-> if p x then failwith "err"
				else x::(p_nopred p tl)
and p_nopred p = function
	[]->failwith "err"
	|t::ts-> try path_no_pred p t with Failure "err" -> p_nopred p ts 

let rec same_str t1 t2 = match (t1, t2) with 
	(Tr(a,[]),Tr(b,[]))-> true
	|(Tr(a,tl1),Tr(b,tl2))-> (List.length tl1 = List.length tl2) && (s_str_lis tl1 tl2)
	|_->false
and s_str_lis l1 l2 = match (l1, l2) with 
	([],[])-> true
	|(a::r1,b::r2)->(same_str a b) && (s_str_lis r1 r2)
	| _-> false

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
let lst = [(Rosso,[1;2;4;7;10]); (Giallo,[3;8;11]);
(Verde,[0;5;6;13]); (Blu,[9;12;14;15])]

let rec radici = function
	[]->[]
	|(Tr(x,_))::rest -> x::radici rest

let rec colore n = function
	[]->failwith "err"
	|(a,b)::rest-> if List.mem n b then a 
					else colore n rest
let root (Tr(x,_))= x
let rec ramo_colorato a lst t = match t with 
	Tr(x,[])-> if x = a then [x]
				else failwith "err"
	|Tr(x,tl)-> let attuale = colore x lst in 
				x::(r_c_list a lst (List.filter (function y-> (colore (root y) lst) <> attuale) tl ))
and r_c_list a lst = function
	[]-> failwith "err"
	|tr::rest-> try ramo_colorato a lst tr 
				with _-> r_c_list a lst rest	

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

let rec foglia_costo y = function
	Tr(x,[])-> if x = y then (x,x)
				else failwith "err"
	|Tr(x,tl)-> let (a,b) = f_c_lis y tl in (a,b+x)
and f_c_lis y = function
	[]-> failwith "err"
	|t::ts-> try foglia_costo y t with _-> f_c_lis y ts

let rec fogli_lis lst = function
	Tr(x,[])->List.mem x lst
	|Tr(x,tl)-> f_l_lis lst tl 
and f_l_lis lst = function
	[]-> failwith "err"
	|t::ts-> fogli_lis lst t && f_l_lis lst ts
