open List;;
exception NoValueFoud;;
exception BadOpcode;;
exception BadStack;;

let add n1 n2 = n1 + n2;;
let mul n1 n2 = n1*n2;;
let div n1 n2 = n1/n2;;

type exp = 
		   Num of int
		 | Var of string
		 | T 
		 | F
		 | Add of (exp*exp)
		 | Mul of (exp*exp)
		 | Div of (exp*exp)
		 | Ifthenelse of (exp*exp*exp)
		 | Lam of (string*exp)
		 | Rlam of (string*string*exp)
		 | App of (exp*exp)
		 | Iszero of exp  

type answer = 
			| N of int
			| B of bool
	and 
closure = VL of (table*answer)
		| CL of (table*(opcode list))
		| FCL of (table*string*(opcode list))
		| RFCL of (table*string*string*(opcode list))
	and
table = (string*closure) list  
and 
opcode =
		| NUM of int
		| VAR of string
		| BOOL of bool
		| ADD
		| MUL 
		| DIV
		| COND of ((opcode list)*(opcode list))
		| ISZERO
		| CLOS of (string * (opcode list))
		| APP of (opcode list)
		| RCLOS of (string * string * (opcode list))

let rec compile (e:exp) =
	match e with
	| Num x -> [NUM x]
	| Var x -> [VAR x]
	| T -> [BOOL true]
	| F -> [BOOL false]
	| Add (e1,e2) -> (compile e1)@(compile e2)@[ADD]
	| Mul (e1,e2) -> (compile e1)@(compile e2)@[MUL]
	| Div (e1,e2) -> (compile e1)@(compile e2)@[DIV]
	| Ifthenelse (e1,e2,e3) -> (compile e1)@[COND ((compile e2),(compile e3))]
	| Iszero e1 -> (compile e1)@[ISZERO]
	| Lam (s,e1) -> [CLOS (s,((compile e1)))]
	| App (e1,e2) -> (compile e1)@[APP (compile e2)]
	| Rlam (s1,s2,e) -> [RCLOS (s1,s2,((compile e)))];;

let rec get x (l:table):closure =
	match l with
	[] -> raise NoValueFoud
	| (s,a)::t -> if s=x then a else get x t;;


let rec krivinemc (s:closure list) (g:table) (c:opcode list) =
	match c with
	| [] -> List.hd s
	| (NUM n)::c' -> krivinemc (VL (g,N n)::s) g c'
	| (BOOL true)::c' -> krivinemc (VL (g,B true)::s) g c'
	| (BOOL false)::c' -> krivinemc (VL (g,B false)::s) g c'
	| (VAR (x))::c' ->(match get x g with 
					 |CL (g',o) -> (let v = krivinemc [] g' o in krivinemc (v::s) g c')
					 | _  -> krivinemc ((get x g)::s) g c')
	| (ADD)::c' -> (match s with
					| (VL (g',N n1))::(VL (g'',N n2))::s' -> krivinemc ((VL (g,N (add n1 n2)))::s') g c'
					| _ -> raise BadStack)
	| (MUL)::c' -> (match s with
					| (VL (g',N n1))::(VL (g'',N n2))::s' -> krivinemc ((VL (g,N (mul n1 n2)))::s') g c'
					| _ -> raise BadStack)
	| (DIV)::c' -> (match s with
					| (VL (g',N n1))::(VL (g'',N n2))::s' -> krivinemc ((VL (g,N (div n2 n1)))::s') g c'
					| _ -> raise BadStack)
	| (COND (e1,e2))::c' -> (match s with
							| (VL (g', B true))::s'  -> krivinemc s' g (e1@c')
							| (VL (g', B false))::s' -> krivinemc s' g (e2@c')
							| _ -> raise BadStack)
	| ISZERO::c' -> (match s with
						 | (VL (g',N 0))::s'-> krivinemc ((VL (g,B true))::s') g c'
						 | _ -> krivinemc ((VL (g,B false))::(List.tl s)) g c')
	| (CLOS (s1,o))::c' -> krivinemc ((FCL (g,s1,o))::s) g c'
	| (RCLOS (s1,s2,o))::c' -> krivinemc ((RFCL (g,s1,s2,o))::s) g c'
	| (APP (o))::c' -> (match s with
					   | (FCL (g',s1,o1))::s' -> let v = krivinemc s' ((s1,CL(g,o))::g') o1 in krivinemc (v::s') g c'
					   | (RFCL (g',s1,s2,o1))::s' -> let v = krivinemc s' ((s1,CL(g,o))::(s2,RFCL(g',s1,s2,o1))::g) o1 in krivinemc (v::s') g c'
					   | _ -> raise BadStack);;


let exp1 = Add (Num 1, Num 2);;  
let exp2 = Add (Num 1, Var "x");; 
let exp3 = Lam ("x", Num 1);;
let exp5 = App (Lam ("x",Num 1), Num 1);;
let exp6 = App (Lam ("x", Add (Var "x" , Num 1)), Num 1);;
let exp7 = App (Rlam ("x","f" ,( Ifthenelse (Iszero (Var "x"), Num 1, Add (App(Var "f" , Add (Var "x", Num (-1))), Num 1)))),Num 10);;
let exp8 = Div (Num 1, Num 2);;
let exp9 = App (Lam ("x",Num 1), Div (Num 2, Num 0));;
let exp10 = App (Lam ("x", Var "x"), Div (Num 2, Num 0));;