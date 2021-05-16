open Lexer;;
open Parser;;
open Eval;;
open List;;
open Lexing;;
exception NoSuchSymbol of string;;
exception EmptyQuery;;
print_string ("Welcome to Prolog (64 bits, version 1.0.0, Ocaml based)\n"); flush stdout;;
print_string ("Created by :- Sanket Gandhi\n"); flush stdout;;
print_string ("Made by student so possible errors\n"); flush stdout;;
print_string ("Any suggestions mail to 'sangandhi29@gmail.com'\n"); flush stdout;;
print_string ("Put your code in 'test.pl'\n\n"); flush stdout;;
print_string ("Reading'test.pl'"); flush stdout;;
print_string ("\n\n"); flush stdout;;


let rec print_af af =
	match af with
	| ((a,c),b) -> print_string (a^"("); print_string (Eval.trmlst_to_string b);print_string ")";;

let rec print_afl afl =
	match afl with
	[] -> begin (); end
	| h::t -> begin print_af h; print_afl t; end;;

let rec print_clause ac =
	match ac with
	| Fact (a) -> begin print_af a; print_newline();flush stdout; end
	| Rule (a,b) ->begin print_af a;print_string(":-");print_afl b; print_newline(); flush stdout; end;;

let rec print_clausel l i=
	if (i > Array.length l - 1) then begin
		();
	end
	else begin print_clause l.(i); print_string (",");print_clausel l (i+1)  ;end  


let rec map2 kb =
	match kb with
	| [] -> begin (); end
	| ((a,b),l)::t -> begin print_string (("(")^a^","); print_int(b);print_string(")\n");flush stdout;
							 print_clausel l 0; map2 t end;;

let refine clause = 
	let rec refine_af af =
		let rec refine_term t =
			match  t with
			| Var x -> Var ("_"^x)
			| Funct af -> Funct (refine_af af)
			| _ -> t in
		match  af with
		| a,b -> (a, (List.map refine_term b)) in
	match clause with
	| Fact a -> Fact (refine_af a)
	| Rule (a,b) -> Rule (refine_af (a), List.map refine_af b);;

let get3 a =
	match a with
	| _,_,c -> c;;


let get2 a =
	match a with
	| _,c,_ -> c;;

let get1 a =
	match a with
	| c,_,_ -> c;;

let rec see_inkb kb s n i =
	if (i > Array.length kb -1) then false,[||],i
	else begin
		match kb.(i) with
		| ((a,b),c) -> begin if (String.compare a s = 0) && b = n then true,c,i else see_inkb kb s n (i+1) end
		end;;

let see_inkb2 kb afl =
	let f kb x= 
	match x with
	| (x1,x2),x3 -> not (get1 (see_inkb kb x1 x2 0)) in
	List.find (f kb) afl;;

(*takes list of clause and make it to kb*)
let rec make_kb kb kbt i =
	match kbt with										(*kbt is lit of cluase*)
	  [] -> ();											
	| h::t1 ->
			  match h with
	            Fact ((s,n),tl) ->
	    							begin 
	  									let t = see_inkb !kb s n 0 in 
										if get1 t then begin !kb.(get3 t) <- ((s,n),(Array.append (get2 t) [|h|])); end
										else begin kb := Array.append !kb [|(s,n),[|h|]|]; end; make_kb kb t1 (i+1); end

			  | Rule (((s,n),tl),afl) -> 
										try 
											let x = see_inkb2 !kb afl in
											match x with
											| ((a1,a2),a3) -> raise (NoSuchSymbol (a1^"/"^string_of_int(a2)))
										with Not_found -> begin
												let t = see_inkb !kb s n 0 in 
												if get1 t then begin 
													!kb.(get3 t) <- ((s,n),(Array.append (get2 t) [|h|])); end
												else begin kb := Array.append !kb [|(s,n),[|h|]|]; end;
												make_kb kb t1 (i+1);
												end;;

let (kb:'a array ref) = ref [||];;

let file_in = open_in "test.pl";;

(*read the code*)
let read_from_file () = 	
	try
				begin
		        let input = Lexing.from_channel file_in in
		        let x =List.map refine (Parser.main_file Lexer.tokens input) in
		        make_kb kb x 0;
		        print_string ("the length of kb ");print_int (Array.length !kb);
		        print_newline();flush stdout;
		       end
	with 
	 Failure s -> begin print_string("false\nError: Invalid Token at line \n"); flush stdout;exit 0; end
	| Parsing.Parse_error -> begin print_string("false\nError: Invalid Syntax at line \n"); flush stdout;exit 0; end
	| NoSuchSymbol i -> begin print_string("false\nError: Invalid Atomic formula "^i^ " \n"); flush stdout;
						 kb := [||];exit 0; end
	| End_of_file -> close_in file_in;;

read_from_file ();;
(* map2 (Array.to_list (!kb));;*)
(*print_string (string_of_int (Array.length (!kb))); flush stdout;;*)

let rec next() = 
	let x = input_char stdin in
	if (x = ';') then  if (input_char stdin = '\n') then true else next ()
	else if (x = '.') then false
	else begin print_string ("Error: Invalid Token\nEnter ';' for next\nEnter '.' to terminate\n"); flush stdout;
	next() end;;

(*now the brain of whole interpretor*)
(*the solver*)
let query_solver kb queryl =
	let (stack:('a list ref)) = ref [] in
	let (subs:('b list ref)) = ref [] in
	let sub_stack = ref [0] in
	let depth = ref 0 in
	let query = ref (Array.of_list queryl) in
	let variables = var_afl queryl in
	let visited = ref (Array.make (List.length queryl) (-1)) in
	let got = ref false in
	let id = ref 0 in
	let ans = ref (final_solution stack kb query variables subs visited depth sub_stack got id) in
	let continiue = ref true in
	while (!continiue) do 
		print_string (String.sub !ans 0 ((String.length !ans) - 1)); flush stdout;
		try 
			ans := final_solution stack kb query variables subs visited depth sub_stack got id;
			continiue := next ();
		with e -> begin  print_newline(); continiue := false; end 
	done;;


(*the main function*)
let rec main() = 
	try
		print_string ("?- "); flush stdout;
		let input = Lexing.from_channel stdin in
		let x = Parser.main_shell Lexer.tokens input in
		try 
			if (List.length x = 0) then raise EmptyQuery
			else begin 
				let y = see_inkb2 !kb x in
				match y with
					| ((a1,a2),a3) -> raise (NoSuchSymbol (a1^"/"^string_of_int(a2))) end
			with 
				| Not_found -> begin query_solver !kb x; main(); end
	with
		   EmptyQuery -> print_string("Done Exisiting shell\n"); exit 0;
		| Eval.StackOverflow -> begin print_string("Fatal Error: StackOverflow \n");flush stdout; main() end;
	    |  Failure s -> begin print_string("Error: Invalid Token at line \n"); flush stdout;main(); end
		| Parsing.Parse_error -> begin print_string("Error: Invalid Syntax \n"); flush stdout;main(); end
		| NoSuchSymbol i -> begin print_string("Error: Invalid Atomic formula "^i^ " \n"); flush stdout; kb := [||];main(); end;;
main();;


(*Rule (af*af list)
Rule ((symbol*term list)*af list)
Rule (((string,int)*term list)*af list)
Rule (((s,n),tl),afl)
Fact ((s,n),tl)*)


(* edge(a,b)W
edge(bYc).
edge(b,d).
edge(d,e).
path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z),path(Z,W),edge(W,Y). 
 *)