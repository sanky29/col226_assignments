open List;;
type symbol = string;;


type variable = string;;

type term = 
			   V of variable 
			 | Node of symbol * (term list);;

exception SymbolRepeatation of string;;
exception InvalidArraity of string;;
exception NoSuchSymbol;;
exception NOT_UNIFIABLE;;

let rec check_in_list s l = 
        match l with
        | [] -> true
        | h::t -> if h = s then false else check_in_list s t;;

let check_sig (l:((symbol*int) list)) =
        let rec temp sym templist =
        	match sym with
        	| [] -> true
        	| (a,b)::t ->
        	 				if b >= 0 then 
        	 						if (check_in_list a templist) then temp t (a::templist)
        	 						else raise (SymbolRepeatation ("the symbol "^a^" is repeated"))
        	 				else raise (InvalidArraity ("the symbol "^a^" is having Invalid arrity of "^(string_of_int b)));
 	        	in temp l [];; 

let rec foldl f l e =
	match l with
	[] -> e
	| h::t -> foldl f t (f e h);;

let rec foldl2 f1 f2 l1 l2 e =
	match l1,l2 with
	| h1::t1,h2::t2 -> foldl2 f1 f2 t1 t2 (f1 e (f2 e h1 h2))
	| _ -> e;;


let rec len l =
	match l with
	[] -> 0
	| h :: t -> 1 + len t;;

let rec get_arrity (l:((symbol*int) list)) c =
	match l with
	| [] -> raise NoSuchSymbol
	| (a,b)::t -> if a = c then b else get_arrity t c;;

let rec wfterm (t:term) (l:((symbol*int) list))=
	let f a b =(wfterm b l)&&a in 
	match t with
	| V x -> true
	| Node (a,c) -> if  len c = get_arrity l a then
							foldl f c true else false;;

let rec ht (t:term) =
	let f a b = max (ht b) a in 
	match t with
	| V x -> 0
	| Node (a,c) -> foldl f c 0 + 1;;

let rec add_in_set l a = 
	match l with
	| [] -> [a]
	| h::t -> if h = a then l else h::(add_in_set t a);;

let rec union l1 l2 =
	match  l1,l2 with
	| [], _ -> l2
	| _,[] -> l1
	| h::t,l -> union t (add_in_set l h);;

let rec var (t:term)=
	let f a b = union (var b) a in
	match t with
	| V x -> [x]
	| Node (a,c) -> foldl f c [] ;;

let rec size (t:term)=
	let f a b = (size b) + a in
	match t with
	| V x -> 1
	| Node (a,c) -> foldl f c 0 + 1 ;;

let rec find_value l a =
	match l with
	| [] -> V a
	| (c,b)::t -> if c = a then b else find_value t a;;

let rec subst s oterm =
	match oterm with
	| Node (a,b) -> Node (a, map (subst s) b)
	| V a -> find_value s a;;  

let rec map2 f l1 l2 =
	match l1,l2 with
	| h1::t1 , h2::t2 -> (f h1 h2)::(map2 f t1 t2)
	| _ -> [];;

let rec substitute_for_one var1 value1 oterm nterm =
	match oterm,nterm with
	| V a, V b -> if a = b then
						if a = var1 then value1
						else V a
				  else V b
	| Node (a1,b1) , Node (a2,b2) -> 
									if (a1 = a2) then Node (a1, map2 (substitute_for_one var1 value1) b1 b2)
									else Node (a2,b2)
	| _,a -> a;;

let rec map l f =
	match l with
	[] -> []
	| h::t -> (f h)::(map t f);;

let rec modified_union l1 l2 l=
	match l2 with
	[] -> l1
	| (a,b)::t -> if (check_in_list a l) then (a,b)::(modified_union l1 t l) else modified_union l1 t l;;

let rec compose s1 s2 =
	let f2 s2 x =
		match x with
		| a,b -> a,(subst s2 b) in
	let l = map s1 (f2 s2) in
	let f x =
		match x with
		| a,_ -> a in
	modified_union l s2 (map l f);;	

let rec mgu t1 t2 =
	let rec t l t1 t2=
		match subst l t1, subst l t2 with
			| V a, V b -> if a != b then compose l [(a,V b)] else l
			| V a, Node (c,d) -> if check_in_list a (var (Node (c,d))) then compose l [(a, Node(c,d))] else raise NOT_UNIFIABLE
			| Node (c,d), V a -> if check_in_list a (var (Node (c,d))) then compose l [(a, Node(c,d))] else raise NOT_UNIFIABLE
			| Node (a,b), Node(c,d) -> if (a=c) then foldl2 compose t b d l else raise NOT_UNIFIABLE
	in
	t [] t1 t2;;

let t = Node ("Plus",[Node ("Pred",[Node ( "Zero",[])] );Node ("Zero",[])]);;

let t2 = Node ("Plus",[Node ("Pred",[V "x"] );V "y"]);;

let t1 = Node ("Plus", [V "x"; Node ("Plus",[Node ("Pred",[V "y"] ); V "x"])]);;

let s1 = ["x", V "a"; "y", Node ("Plus", [V "b"; V "z"]); "z", V "x"];;

let s2 = ["x", V "w"; "y", V "z"; "z", V "a"];;
