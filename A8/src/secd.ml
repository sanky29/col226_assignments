exception NoValueFoud;;
exception BadOpcode;;
exception BadStack;;

let add n1 n2 = n1 + n2;;
let mul n1 n2 = n1*n2;;

type exp = 
		   Num of int
		 | Var of string
		 | T 
		 | F
		 | Add of (exp*exp)
		 | Mul of (exp*exp)
		 | Ifthenelse of (exp*exp*exp)
		 | Lam of (string*exp)
		 | Rlam of (string*string*exp)
		 | App of (exp*exp)
		 | Iszero of exp  


type answer = 
			| N of int
			| B of bool
			| Vcl of (table*string*(opcode list))
			| Rvcl of (table*string*string*(opcode list))
	and 
table = (string*answer) list  
and 
opcode =
		| NUM of int
		| VAR of string
		| BOOL of bool
		| ADD
		| MUL 
		| COND of ((opcode list)*(opcode list))
		| ISZERO
		| RET
		| CLOS of (string * (opcode list))
		| APP
		| RCLOS of (string * string * (opcode list))

let rec get x (l:table):answer =
	match l with
	[] -> raise NoValueFoud
	| (s,a)::t -> if s=x then a else get x t;;

let rec compile (e:exp) =
	match e with
	| Num x -> [NUM x]
	| Var x -> [VAR x]
	| T -> [BOOL true]
	| F -> [BOOL false]
	| Add (e1,e2) -> (compile e1)@(compile e2)@[ADD]
	| Mul (e1,e2) -> (compile e1)@(compile e2)@[MUL]
	| Ifthenelse (e1,e2,e3) -> (compile e1)@[COND ((compile e2),(compile e3))]
	| Iszero e1 -> (compile e1)@[ISZERO]
	| Lam (s,e1) -> [CLOS (s,((compile e1)@[RET]))]
	| App (e1,e2) -> (compile e1)@(compile e2)@[APP]
	| Rlam (s1,s2,e) -> [RCLOS (s1,s2,((compile e)@[RET]))];;

let rec stkmc (s: answer list) (g:table) (c:opcode list) d =
	match (s,c) with
	| _, [] -> (s,g)
	| _,(NUM n)::c' -> stkmc ((N n)::s) g c' d
	| _,(BOOL b)::c' -> stkmc ((B b)::s) g c' d
	| _,(VAR x)::c' -> stkmc ((get x g)::s) g c' d
	| (N n1)::(N n2)::s', ADD::c' -> stkmc ((N (add n1 n2))::s') g c' d
	| (N n1)::(N n2)::s', MUL::c' -> stkmc ((N (mul n1 n2))::s') g c' d
	| v::s',RET::c' -> (match d with
				| [] -> (v::s,g);
				| (s'',g'',c'')::d'' -> stkmc (v::s'') g'' c'' d'')
	| (B b)::s',(COND (o1,o2))::c' -> if b then stkmc s' g (o1@c') d
									else stkmc s' g (o2@c') d
	| (N n1)::s',(ISZERO::c') -> stkmc ((B (n1 = 0))::s') g c' d
	| _, (CLOS (x,o))::c' -> stkmc ((Vcl (g,x,o))::s) g c' d
	| _, (RCLOS (s1,s2,o))::c' -> stkmc ((Rvcl (g,s1,s2,o))::s) g c' d
	| a::(Rvcl (g',s1,s2,o))::s' , APP::c' -> stkmc [] ((s1,a)::(s2,Rvcl (g',s1,s2,o))::g') o ((s',g,c')::d)  
	| a::(Vcl (g',x,o))::s',APP::c' -> stkmc [] ((x,a)::g') o ((s',g,c')::d)
	| _ -> raise BadStack;;


let exp1 = Add (Num 1, Num 2);;  
let exp2 = Add (Num 1, Var "x");; 
let exp3 = Lam ("x", Num 1);;
let exp5 = App (Lam ("x",Num 1), Num 1);;
let exp6 = App (Lam ("x", Add (Var "x" , Num 1)), Num 1);;
let exp7 = App (Rlam ("x","f" ,( Ifthenelse (Iszero (Var "x"), Num 1, Add (App(Var "f" , Add (Var "x", Num (-1))), Num 1)))),Num 10);;