(*define type of prolog code*)
(*need to define recursive nature in terms and af*)
open List;;
open String;;
open Array;;

(*variable and constant*)
type variable = string;;
type constant = string;; 
type symbol = string*int;;

(*define term using variable and constant*)
type term =
		  Var of variable
		| Const of constant
		| Funct of af
		and af = (symbol*(term list));;

(*defined clause*)
type clause = 
		  Fact of af
		| Rule of (af*(af list));;

(*need to define knowledge base*)
type kb = (string*int*(clause list) array);;



exception Nomgu;;
exception NoUnion;;
exception NoSuchSymbol;;
exception NotValidLeaf;;
exception NotValidParent;;
exception NoFurtherSolution;;
exception ErrorInAns;;
exception RuleFound of clause;;
exception StackOverflow;;

(*the whole knowledge is represended above*)

  let f x =
            let n = String.length x in
            String.sub x 0 (n-1);;
(*the useful functions for tuple opreations*)
let get1 x = 
	match  x with
	 a,b -> a;;
let get2 x = 
	match  x with
	 a,b -> b;;

(*union of set*)
let rec union_set f l x =
	if l = [] then [x]
else if (List.exists (f x) l) then x::l
	else l;;

(*union of set*)
let rec union_list f l1 l2 =
	let _f l x = union_set f l x in
	List.fold_left _f l1 l2;;

(*the comparison function for var*)
let fvar (x1:variable) (x2:variable) =
	String.compare x1 x2 =0;;

let fvar2 (x1:variable) (x2:variable) = not (fvar x1 x2);;
(*the union of subst*)

let rec var_list l1 =
	match l1 with
	  [] -> []
	| h::t -> union_list fvar2 (var_over_term h) (var_list t) and
var_over_term t =
	match t with
	  Const c -> []
	| Var x -> [x]
	| Funct (a,b) -> var_list b;;

(*find variables in term*)
let rec var (t:af) =
	match t with
	a,b -> var_list b;;

(*var over af list as*)
let var_afl afl =
	let _f c x = union_list fvar2 c (var x) in
	List.fold_left _f [] afl;;

(*now write function for subst as*)
let rec subst s a:af = 
	match a with
	(a1,b1) -> (a1,List.map (subst_on_term s) b1) and
    subst_on_term s t =
		match t with
		  Var x   -> begin try List.assoc x s with Not_found -> Var x end
		| Const x -> Const x
		| Funct x -> Funct (subst s x);;

(*now we need to make list of substitutions as*)
let rec subst_many al a:af =
	match al with
	| [] -> a
	| h::t -> subst_many t (subst h a);;

(*identify if given two terms are same or not*)
let rec same t1 t2 =
	let rec map2 f l1 l2 =
		match l1,l2 with
		  [],[] -> true
		| h1::t1,h2::t2 -> if f h1 h2 then map2 f t1 t2
						   else false
		| _ -> false in
	match t1,t2 with
      Var x1 , Var x2 -> x1=x2
    | Const c1 , Const c2 -> c1=c2
    | Funct (a1,b1), Funct(a2,b2) ->if a1=a2 then map2 same b1 b2
    							    else false
    | _ -> false;;

let rec fterm x1 x2 =
	match x2,x1 with
	| (a1,b1),(a2,b2) -> if a1=a2 then if same b1 b2 then false else raise NoUnion
				  else true;;
(**)
(*given two af find their mgu*)
(*the mgu is recursive function*)
let rec mgu af1 af2=
	let mgu_on_term t1 t2 =
		match t1,t2 with
		  Const c1,Const c2 -> if compare c1 c2 != 0  then raise Nomgu else []
		| Var x1, Var x2 ->  [x1,t2]
		| Var x1, Const c2 -> [x1,t2]
		| Const c1, Var x2 -> [x2,t1]
		| Funct a1, Var x2 -> if not (List.exists (fvar x2) (var a1)) then [x2,t1] else raise Nomgu 
		| Var x1, Funct a2 -> if not (List.exists (fvar x1) (var a2)) then [x1,t2] else raise Nomgu 
		| Funct a1, Funct a2 -> mgu a1 a2
		| _ -> raise Nomgu in
	let rec list_mgu l1 l2 = 
		match l1,l2 with
		  [],[] -> []
		| h1::t1,h2::t2 -> (mgu_on_term h1 h2)::(list_mgu t1 t2)
		| _ -> raise Nomgu in
	match af1,af2 with
	(a1,b1),(a2,b2) -> if compare (get1 a1) (get1 a2) != 0 then raise Nomgu else if List.length(b1) != List.length(b2) then raise Nomgu
					   else List.fold_left (union_list fterm) [] (list_mgu b1 b2);;

(*one more checker*)
let check_var a b =
	match b with
	| Var x -> ((String.compare a x) != 0)
	| _ -> true;;

(*the function for comapring two set union*)
let fterm2 x1 x2 =
	match x2,x1 with
	| (a1,b1),(a2,b2) -> 
	if a1=a2 &&  (( check_var a1 b1 && check_var a2 b2) && not (same b1 b2)) then true else false;;

(*to validate the two substitutions*)
let valid_set l1 l2 f =
	let _f l x1 x2 = (not (List.exists (f x2) l)) && x1 in
	List.fold_left (_f l1) true l2;;

(*validity over all sets*)
let rec valid_overall l x f =
	let _f x c y = c&&(valid_set x y f) in
	List.fold_left (_f x) true l;;

(*changing variable in clause with depth*)
let append_depth c d sl=
	let s = "_"^(string_of_int sl)^"_"^(string_of_int d) in
	let rec change_af a =
		let change_term t =
			match t with
			| Var x -> Var (x^s)
			| Funct af -> Funct (change_af af)
			| _ -> t in
		match a with
		| sym,trml -> sym,(List.map change_term trml) in
	match c with
	  Fact a -> Fact (change_af a)
	| Rule (a,b) -> Rule (change_af a, List.map change_af b);;  


(*remove first k elements in list*)
let rec remove l k =
	match l with
	| h::t -> if (k=0) then l else remove t (k-1)
	| [] -> [];; 

let rec find_inkb kb s i=
	if (get1 (kb.(i)) = s) then get2 (kb.(i))
	else find_inkb kb s (i+1);;

(*find suitable mgu*)
let rec find_subst x l v d sl =
	if (Array.length(l) - 1 <= v.(d)) then raise NotValidLeaf
	else
			try
				let temp = append_depth l.(v.(d)+1) d sl in
			   match temp with
				| Fact a -> begin ((mgu x a),temp) end 
				| Rule (a,b)-> begin  ((mgu x a),temp) end
			with e -> 
				begin v.(d) <- v.(d)+1;
				find_subst x l v d sl end;;

(*join the query*)
let join query add i =
	let t2 = (Array.sub query (i+1) ((Array.length query) - 1 - i)) in
	(Array.append add t2);; 

(*modify visited*)
let rec modify_visitd v b d=
	let t = Array.sub v (d+1) ((Array.length v) - 1 - d) in
	let t3 = Array.make b (-1) in 
	(Array.append t3 t);;


(*term to string*)
let rec term_to_string t =
	match t with
	  Const c -> c
	| Var x -> x
	| Funct ((a,b),tl) -> a^"("^(trmlst_to_string tl)^")"
and trmlst_to_string tl=
	match tl with
	[] -> ""
	| h::t -> (term_to_string h)^","^(trmlst_to_string t);;

let rec print_af af =
	match af with
	| ((a,c),b) -> print_string (a^"("); print_string (trmlst_to_string b);print_string ")";;

let rec print_afl afl =
	match afl with
	[] -> begin (); end
	| h::t -> begin print_af h; print_afl t; end;;


let rec forward kb query subs visited depth sub_stack sl=
	(* print_string("query is ");
	print_newline(); 
	print_afl (Array.to_list !query); 
	print_newline(); 
	flush stdout;	 *)
	let t1 = Array.length(!query) - !depth -1 in
	if (!depth < 0) then raise NotValidLeaf
	else if (t1 < 0) then
		begin (); end
	else 
		let t2 = subst_many !subs ((!query).(!depth)) in
		(* print_string("after subs \n");
		print_afl (Array.to_list !query); 
		print_newline(); 
		print_newline(); 
		flush stdout;  *)
		let t3 = find_inkb kb (get1 t2) 0 in
		let t4 = find_subst t2 t3 !visited !depth sl in
		!visited.(!depth) <- !visited.(!depth) + 1;
		match get2 t4 with
		  Fact a ->begin if (valid_overall !subs (get1 t4) fterm2) then
		  			begin 
		  				subs := [(get1 t4)]@(!subs);
		  				depth := !depth + 1;
		  				sub_stack := (List.length (get1 t4))::(!sub_stack);
		  				forward kb query subs visited depth sub_stack sl;
		  			end
		  			else forward kb query subs visited depth sub_stack sl end
		| Rule (a,b) -> begin if (valid_overall !subs (get1 t4) fterm2) then 
								raise (RuleFound (get2 t4))
		  			else forward kb query subs visited depth sub_stack sl end;;

let rec backward kb query subs visited depth sub_stack =
	if (!depth = -1) then raise NoFurtherSolution
	else  (*look for the length of the node*)  (*then just check visisted*)
	      begin
	      	let t1 = Array.length (find_inkb kb (get1 (!query.(!depth))) 0) in 
		    if (!visited.(!depth) >= t1 - 1) then begin
      			!visited.(!depth) <- -1;
      			depth := !depth - 1;
      			subs := remove !subs (List.hd !sub_stack);
      			sub_stack := List.tl (!sub_stack);
      			backward kb query subs visited depth sub_stack;
		 		end
		 	else begin();end     		
		end;;

(*iteratively find new substitution*)
let rec subst_on_term_rec s t =
	let ans = subst_on_term s t in
	if ans = subst_on_term s ans then ans
	else subst_on_term_rec s ans;; 


(*apply the substituions as follows*)
let rec apply_subst var subs = 
	let f x y = subst_on_term_rec y x in 
	let _f l x = List.fold_left f x l in
	List.map (_f subs) var;;
let rec ans_to_string (vars:variable list) ans =
	match vars,ans with
	| [],[] -> ""
	| _::_ , [] -> raise ErrorInAns
	| [] , _::_ -> raise ErrorInAns
	| h1::t1, h2::t2 ->
	begin
		match h2 with
		| _ -> h1^"="^(term_to_string h2)^","^(ans_to_string t1 t2)
	end;;

(*filter the ans as*)
let rec filter l =
	match l with
	  [] -> []
	| (v,trm)::t -> if String.get v 0 = '_' then filter t else (v,trm)::(filter t);;

let rec search kb query subs visited depth sub_stack sl=
	try 
		forward kb query subs visited depth sub_stack sl;
		depth := !depth - 1;
	let f x = x in
	let t = List.map f (!subs) in
	subs := List.tl (!subs);
	sub_stack := List.tl (!sub_stack);
	begin
		try
			backward kb query subs visited depth sub_stack;
			List.rev t
		with NoFurtherSolution -> List.rev t
	end
	with NotValidLeaf -> begin
	backward kb query subs visited depth sub_stack;
	search kb query subs visited depth sub_stack sl; end;; 

let rec final_solution stack kb query var subs visited depth sub_stack got sl=
	if(!sl > 512) then raise StackOverflow
	else begin
	sl := !sl + 1;
	try 
		let t = search kb query subs visited depth sub_stack !sl in
		got := true;
		let f x = Var x in
		let t2 = ans_to_string var (apply_subst (List.map f var) t) in
    	if (String.length t2 = 0) then "true" else t2
    with
    	NoFurtherSolution -> begin if(not !got) then 
    						   begin  match !stack with
	 	  						[] -> begin got := true; "false" end 
								| h::t -> 
									begin match h with
									(a1,a2,a3,a4,a5,a6) -> begin (* (!query,!subs,!visited,!depth,!sub_stack):*)
														stack := t;query := a1;subs := a2;visited := a3;depth := a4;sub_stack:= a5;got:= a6 || !got; 
														if (List.length var = 0) then "false" else
														final_solution stack kb query var subs visited depth sub_stack got sl;
									   						end
									end
								end
							else begin
								match !stack with
								[] -> raise NoFurtherSolution
								| h::t -> 
									begin match h with
									(a1,a2,a3,a4,a5,a6) -> begin (* (!query,!subs,!visited,!depth,!sub_stack):*)
														stack := t;query := a1;subs := a2;visited := a3;depth := a4;sub_stack:= a5;got:=a6; 
														final_solution stack kb query var subs visited depth sub_stack got sl;
									   						end
									end
							end
							end
		| RuleFound b -> begin
			match b with
			| Rule(hd,bd) -> begin
								stack := (!query,!subs,!visited,!depth,!sub_stack,!got)::(!stack);
								let uni = mgu hd (subst_many !subs !query.(!depth)) in 
								let new_subs = filter uni in
								query := join !query (of_list bd) (!depth);
								query := Array.map (subst_many (uni::(!subs))) (!query);
								subs := (new_subs::(!subs));
								visited := modify_visitd !visited (List.length bd) !depth;
								sub_stack := (List.length new_subs)::(!sub_stack);
								depth := 0;
								got := !got && (List.length var != 0);
								final_solution stack kb query var subs visited depth sub_stack got sl end
			| _ -> raise NoFurtherSolution		
		end
	end;;
(* 

let kb = [|("father",1),[|Fact (("father",1),[Const "sanjay"]); Fact (("father",1),[Const "bansilal"])|]; ("mother",1),[|Fact (("mother",1),[Const "suvarna"])|]|];;
let query = ref [|("father",1),[Var "X"]; ("mother",1),[Var "Y"]|];;
let query = ref [|("father",1),[Var "X"]; ("mother",1),[Const "sd"]|];;
let visited = ref [|-1;-1|];;
let kb = [|("parent",1),[|Fact (("parent",1),[Const "sanjay"])|]; ("male",1),[|Fact (("male",1),[Const "sanjay"])|];("father",1),[|Rule ((("father",1),[Var "_X"]),[("male",1),[Var"_X"];("parent",1),[Var"_X"]])|]|];;
let query = ref [|("father",1),[Var "X"]|];;
let  query = ref [|("father",1),[Var "X"]|];;
let kb = [|("ok",1),[|Fact (("ok",1),[Const "e"]); Rule ((("ok",1),[Var "X"]),[("ok",1),[Var "X"]])|]|];;
let query = ref [|(("ok",1),[Var "X"])|];;
let sub_stack = ref [0] ;;
let depth = ref 0;;
let visited = ref [|-1|];; 

let kb = [|(("edge",2),[|Fact (("edge",2),[Const "a";Const "b"]);
					     Fact (("edge",2),[Const "b";Const "c"]);
					     Fact (("edge",2),[Const "b";Const "d"]);
					     Fact (("edge",2),[Const "d";Const "e"]);|]);
			(("path",2),[|Rule ((("path",2),[Var "_X";Var "_Y"]),[(("edge",2),[Var "_X";Var "_Y"])]);
						  Rule ((("path",2),[Var "_X";Var "_Y"]),[(("path",2),[Var "_X";Var "_Z"]);(("path",2),[Var "_Z";Var "_Y"])])|])

		|];;
let query = ref [|("path",2),[Const "b"; Const "e"]|];;
let queryl = [("path",2),[Const "b"; Const "e"]];;
let (stack:('a list ref)) = ref [];;
	let (subs:('b list ref)) = ref [];;
	let sub_stack = ref [0];;
	let depth = ref 0;;
	let variables = var_afl queryl;;
	let visited = ref (Array.make (List.length queryl) (-1));;
	let var = var_afl queryl;;
	let got = ref false;;
	let sl = ref 0;;

	let queryl = [("edge",2),[Var "X"; Var "Y"];("edge",2),[Var "Y"; Var "X"]];;
let query = ref [|("edge",2),[Var "X"; Var "Y"];("edge",2),[Var "Y"; Var "X"]|];;
*)
(*forward search in space as*)
(*	kb: knowledge base
	query: query list
	path: the facts or rules used list
	subs: current substitutions
	visitd: the 2d array of visited
	depth: depth of serach*)
(* let rec forward kb query path subs visited depth=
	let t1 = List.length(!query) - depth -1 in
	if (t1 < 0) then
		subs
	else 
		let t2 =subst subs (List.nth (!query) t1) in
		let t3 = List.assoc (get1 t2) kb in
		let t5 = index_ele kb (get1 t2) 0 in
		let t6 = ref 0 in
		let t4 = find_subst t2 t3 (!visited).(depth) t6 in
		match get2 t4 with
		  Fact a ->begin if (valid_set subs (get1 t4) fterm2) then
		  			begin 
		  				path := ([|t5;!t6+1,List.length (get1 t4)|])::(!path);
		  				forward kb query path ((get1 t4)@subs) visited (depth+1)
		  			end
		  			else raise NotValidLeaf end
		| Rule (a,b) -> begin if (valid_set subs (get1 t4) fterm2) then 
							begin
								visited := modify_visitd (!visited) b kb depth;
								path := ([|t5;!t6+1,0|])::(!path);
								query := join !query (List.rev b) t1;
								forward kb query path ((get1 t4)@subs) visited depth end
		  			else raise NotValidLeaf end;;
(*  *)

(*the union of subst*)
let rec fterm x1 x2 =
	match x2,x1 with
	| (a1,b1),(a2,b2) -> if a1=a2 then if same b1 b2 then false else raise NoUnion
				  else true;;

(*now backward search as follows*)
let rec backward kb query path subs visited depth=
	if (visited.(depth).(Array.length (visited.depth) -1) = 1) then 
	begin
		match !path with
		  [] -> raise NoFurtherSolution
		| h::t -> begin let t = List.nth (List.nth kb h.(0)) h.(1) in 
				match t with
				| Fact a -> begin
					(!visited).(depth) <- Array.make (Array.length ((!visited).depth)) 0;
					path := t;
					backward kb query path (remove subs h.(2)) visited (depth-1) 
				end
				| Rule(a,b) -> 
		end
	end *)
(*let rec backward kb query path subs visited =
*)




(*search over one atomic formula*)
(*	af: the list of query
	kb: knowledge base
	state: the stack of search space
	sub: the path of substitution
	back: if we are backtracking or not *)
(* let rec search af kb state sub back =
	if (not back) then			(*forward search*)
	begin
		let t1 = length(af) - length(state) - 1  in
		if (t1>=0) then
			let t2 = ref subst (find_kb (nth t1) kb) sub in
			try  
				let l1 = find_subst (subst sub !t2) kb in
				search af kb (get2 l1) () 0
			with e ->
 *)


(*the examples*)
(* let kb = [("father",1),[Fact (("father",1),[Const "sanjay"]); Fact (("father",1),[Const "bansilal"])]; ("mother",1),[Fact (("mother",1),[Const "suvarna"])]];;
let query = ["father",[Var "X"]; "mother",[Var "Y"]];;
let kb = [("parent",1),[Fact (("parent",1),[Const "sanjay"])]; ("male",1),[Fact (("male",1),[Const "sanjay"])];("father",1),[Rule (((("father",1),[Var "_X"]):af),[("male",1),[Var"_X"];("parent",1),[Var"_X"]])]];;
let query = [("father",1),[Var "X"]];;
let visited = [|[|0|];[|0|];[|0|]|];;
let s = ["X", Const "sanket"; "Z", Const "gandhi"];;
let s2 = ["X", Const "sanket"; "Z", Const "andhi"];;
let (t:af) = ("father",1),[Funct (("male",1),[Var "X"]); Funct (("parent",1),[Var "Y"])];; 
let (t2:af) = ("father",1),[Funct (("male",1),[Const "x"]); Funct (("parent",1),[Const "x"])];; 
let af_list = [("father",1),[Funct (("female",1),[Var "X"]); Funct (("parent",1),[Var "Y"])];t2;t];;


(*find suitable mgu*)
let rec find_subst x l v i=
	match l with
	  [] -> raise NotValidLeaf
	| h::t -> begin
		if (v.(!i) = 1) then begin i:=!i+1; find_subst x t v i end
		else
			try 
				v.(!i) <- 1;
				match h with
				| Fact a -> ((mgu x a),h)
				| Rule (a,b)-> ((mgu x a),h)
			with e -> 
				begin i:= !i+1;
				find_subst x t v i end
	end;;


(*find index of element in list*)
let rec index_ele l x i =
	match l with
	| (a,b)::t -> if a = x then i else index_ele t x (i+1)
	| [] -> i;;


(*join the query*)
let rec join query add i =
	match query with
	h::t -> begin
				if (i=0) then add@t
				else h::(join t add (i-1))  
			end
	| [] -> add;;

(*modify visited*)
let rec modify_visitd v b kb d=
	let t = Array.sub v (d+1) ((Array.length v) - 1 - d) in 
	let t2 = Array.sub v 0 d in
	let t3 = ref ([||]) in 
	let rec f t3 kb b =
		match b with
		 [] -> ()
		| (a1,b1)::t -> begin let t4 = List.assoc a1 kb in 
					t3 := Array.append [|(Array.make (List.length(t4)) 0)|] !t3;
					f t3 kb t; end in 
	begin f t3 kb b;
	t3 := Array.append t2 !t3;
	Array.append !t3 t end;;
 *)







(*quey array of queries
subs: list subs
visited is int array
dpeth
kb is (a*b array)array *)

(* subs := [(get1 t4)]@(!subs);
								stack := (!query,!subs,!visited,!depth,!sub_stack)::(!stack);
								query := join !query (of_list (List.map (subst_many !subs) b)) (!depth);
								subs := [];
								visited := modify_visitd !visited (List.length b) !depth;
								sub_stack := (List.length (get1 t4))::(!sub_stack);
								depth := 0;
								forward stack kb query subs visited depth sub_stack end
 *)


(* match !stack with
	 	  [] -> raise NoFurtherSolution
		| h::t -> 
				begin match h with
				(a1,a2,a3,a4,a5) -> begin *)(* (!query,!subs,!visited,!depth,!sub_stack):*)
										(* 	stack := t;query := a1;subs := a2;visited := a3;depth := a4;sub_stack:= a5;
									end
				end *)