open List;;


exception InvalidInput;;
exception UnequalVectorSize;;
exception UnequalMatrixShape;;
exception IncompatibleMatrixShape;;
exception SingularMatrix;;

type vector = float list;;
type matrix = float list list;;

let rec vdim (x:vector) =
  match x with
  [] -> 0
  | x::t -> 1 + vdim t;;

  let rec mkzerov n:vector =
  		if n < 0 then raise InvalidInput
  		else if n = 0 then []
  		else 0.0::(mkzerov (n-1));; 

let rec iszerov (v:vector) =
	match v with
	[] -> true
	| h::t -> if h = 0.0 then iszerov t 
			  else false;;

let rec addv (v1:vector) (v2:vector) =
	if (vdim v1 != vdim v2) then raise UnequalVectorSize
	else
	match v1,v2 with
	| h0::t0 , h1::t1 -> (h0+.h1)::(addv t0 t1)
	| [], [] -> ([]:vector)
	| _ -> raise UnequalVectorSize;;

let rec scalarmultv c (v:vector):vector =
	match v with
	| [] -> []
	| h::t -> (c*.h)::(scalarmultv c t);;
		

let rec dotprodv (v1:vector) (v2:vector) =
	if (vdim v1 != vdim v2) then raise UnequalVectorSize
	else
	match v1,v2 with
	| h1::t1,h2::t2 -> (h1*.h2) +. dotprodv t1 t2
	| _ -> raise UnequalVectorSize;;

let rec len l =
	match l with
	[] -> 0
	| h::t -> 1 + len t;;

let rec checkm (m:matrix) n1 n2=
	if len m != n1 then false
	else let rec temp m i=
				if m = [] && i = n1 then true
				else match m with
				| [] -> false
				| h::t -> (len h = n2) && (temp t (i+1))
			in
			temp m 0;; 

let mdim (m:matrix) =
	match m with
	| [] -> 0,0
	| h::t -> let n1 = len m in
			  let n2 = len h in
			  if (checkm m n1 n2) then n1,n2
			  else  raise IncompatibleMatrixShape;;

let rec makezerol m =
		if m = 0 then []
		else 0.0::(makezerol (m-1));;

let rec mkzerom n m:matrix=
		if m < 0 || n < 0 then raise InvalidInput
		else
			if n = 0 then []
			else (makezerol m)::(mkzerom (n-1) m);; 

let rec iszerol l =
		match l with
		[] -> true
		| h::t -> if h=0.0 then iszerol t
				  else false;;

let iszerom (m:matrix)=
        if checkm m (len m) (len (hd (m))) then
            match m with
            [] -> true
            | h :: t -> let rec temp a =
			                match a with
			                [] -> true
			                | h1::t1 -> if iszerol h1 then temp t1
			                            else false
                        in
                        temp (h::t)
        else raise IncompatibleMatrixShape;;

let rec mkidlist n i o l=
		if i = n then l
		else if i = o then mkidlist n (i+1) o (1.0::l)
		else mkidlist n (i+1) o (0.0::l);;

let mkunitm m:matrix =
	if m < 0 then raise InvalidInput
	else 
		let rec temp n =
			if n=0 then []
			else (mkidlist m 0 (n-1) [])::(temp (n-1))
		in
		temp m;; 
	 
let rec isidlist i o l=
		match l with
		| [] -> true
		| h::t -> if (i=o && h=1.0) || (i!=o && h = 0.0) then isidlist (i+1) o t
				  else false;;

let isunitm (m:matrix) =
	let n = len m in
	if checkm m n n then
		match m with
		[] -> true
		| h::t -> let rec temp l i=
					match l with
					[] -> true
					| h::t -> isidlist 0 i h &&(temp t (i+1))
			      in
			      temp m 0
	else raise IncompatibleMatrixShape;;

let rec addlist m1 m2 =
	match m1,m2 with
	[],[] -> []
	|h1::t1,h2::t2 -> (h1+.h2)::(addlist t1 t2)
	|_-> [];;

let addm (m1:matrix) (m2:matrix):matrix =
	let n1 = len m1 in
	let n2 = len m2 in
	let n12 = len (hd m1) in
	let n22 = len (hd m2) in
	if checkm m1 n1 n12 && checkm m2 n2 n22 then
		if (n1 = n2)&&(n12 = n22) then
			let rec temp t1 t2 =
				match t1,t2 with
				[],[] -> []
				|h1::t11,h2::t22 -> (addlist h1 h2)::temp t11 t22
				|_-> []
			in temp m1 m2
	    else raise UnequalMatrixShape
	else raise IncompatibleMatrixShape;;

let rec mullist m c =
	match m with
		[] -> []
		|h1::t1-> (h1*.c)::(mullist t1 c);;

let scalarmultm c1 (m2:matrix):matrix =
	let n2 = len m2 in
	let n12 = len (hd m2) in
	if checkm m2 n2 n12 then
		let rec temp t1 c1 =
			match t1 with
			[] -> []
			|h1::t11 -> (mullist h1 c1)::temp t11 c1
		in temp m2 c1
	else raise IncompatibleMatrixShape;;

let get1 x =
	match x with
	|r,_ -> r;;

let get2 x =
	match x with
	|_,r -> r;;

let rec split m=
		match m with
		[] -> [],([]:matrix)
		| [h]::[] -> [h],[]
		| (h::tl)::tl2 ->let n = split tl2 in
						 h::(get1 n), tl::(get2 n)
		|_ -> [],[];;  

let transm (m:matrix):matrix =
	let o = len (hd m) in
	let y = len m in
	if checkm m y o then
		match m with
		[] -> []
		| _ -> let rec temp n i =
					if i=o then []
					else let r = split n in
						 (get1 r)::temp (get2 r) (i+1)
				in
				temp m 0
	else raise IncompatibleMatrixShape;;

let rec product v1 v2 =
	match v1,v2 with
	| h1::t1,h2::t2 -> (h1*.h2) +. product t1 t2
	| _ -> 0.0;;

let rec first_list h m =
	match m with
	| []::tl -> []
	| _ -> let r = split m in
		   (product h (get1 r))::first_list h (get2 r);;

let multm (m1:matrix) (m2:matrix):matrix =
	let n11 = len m1 in
	let n12 = len (hd m1) in

	let n21 = len m2 in
	let n22 = len (hd m2) in
	if checkm m1 n11 n12 && checkm m2 n21 n22 then
		if (n21 = n12) then 
			let rec temp m1 m2 =
				match m1 with
				[] -> [] 
				| h::t -> (first_list h m2)::temp t m2
			in
			temp m1 m2
		else raise UnequalMatrixShape
	else raise IncompatibleMatrixShape;;

let rec printm m =
	match m with 
		| [] -> ()
		| (h::t)::t1 -> print_string ((string_of_float h)^" "); printm (t::t1)
		| []::t1 -> print_string ("\n"); printm (t1);;

(* 
let rec detm2 (m:matrix) =
	let n = len m in
	if n > 0 then 
	  let n1 = len (hd m) in
	  if (checkm m n n1) then
		if (n1 = n) then
			match m with
			| [] -> 0.0
			| [h]::[] -> h
			| h::t -> let rec temp h1 t1 i =
						match h1 with
						[] -> 0.0
						| l::o -> let c = detm (split3 t1 i) in 
						((-1.0)**(float_of_int i))*.l*.c +. temp o t1 (i+1)
					  in 
					  temp h t 0
	    else raise UnequalMatrixShape 
	else raise IncompatibleMatrixShape
	else 0.0;;
 *)
let rec fndnzrow m i c=
	match m with
	| [] -> -1,0.0
	| h::t ->let y = nth h i in
			 if y <> 0.0 then c,y else fndnzrow t i (c+1);;

let rec changeithelement m i h1 c=
	match m with
	[] -> []
	| h::t -> if i = c then h1::t else h::(changeithelement t i h1 (c+1));;

let rec swap m n1 n2 =
	let r1 = nth m n1 in 
	let r2 = nth m n2 in 
	let x = changeithelement m n1 r2 0 in 
	let y = changeithelement x n2 r1 0 in 
	y;;

let rec dividel h1 c =
	match h1 with
	[] -> []
	| h::t -> (h/.c) :: (dividel t c);;

let rec sub h1 h2 =
	match h1,h2 with
		[],[] -> []
		| h::t,a::b -> (h -. a) :: (sub t b)
		| _ -> [];;

let rec mkcolumunit m i j c k inv kinv=
	match m,inv with
	[],[] -> [],[]
	| h::t,h1::t1 -> let l = mkcolumunit t i j (c+1) k t1 kinv in
					 if i = c then 
								   h::get1 l,h1::get2 l
			   		 else let y = nth h j in sub h (mullist k y)::get1 l, sub h1 (mullist kinv y)::get2 l
	| _ -> [],[];;


let rec mkcolumunitd m i j c k=
	match m with
	[] -> []
	| h::t -> let l = (mkcolumunitd t i j (c+1) k) in
					 if i = c then h::l
			   		 else let y = nth h j in (sub h (mullist k y))::l;;


let detm m =
	let n1 = len m in
	if n1 > 0 then let n2 = len (hd m) in 
		if checkm m n1 n2 then
			if n1 = n2 then
				let d = ref 1.0 in
				let rec temp m i d =
					let ans = ref m in
					if i = n1 then d := !d
					else let c = fndnzrow m 0 i in 
						if get1 c = -1 then d := 0.0
						else 
												ans := swap m i (get1 c);
												let c1 = nth !ans i in
												ans := changeithelement !ans i (dividel c1 (nth c1 i)) 0;
												d := !d*.(nth c1 i);
												let c11 = nth !ans i in
												let l = mkcolumunitd !ans i i 0 c11 in
												ans := l;
												temp !ans (i+1) d
				in temp m 0 d;
				!d
			else raise IncompatibleMatrixShape
	 	else raise IncompatibleMatrixShape 
	else 0.0;;

let invm m:matrix =
	let n1 = len m in
	if n1 > 0 then let n2 = len (hd m) in 
		if checkm m n1 n2 then
			if n1 = n2 then
				if detm m <> 0.0 then
					let inve = ref (mkunitm n1) in
					let rec temp m i inv =
						let ans = ref m in
						if i = n1 then !inv 
						else let c = fndnzrow m 0 i in 
							print_string ((string_of_int (get1 c))^"\n");
							if get1 c = -1 then raise SingularMatrix
							else 
													ans := swap m i (get1 c);
													inv := swap !inv i (get1 c);
													let c1 = nth !ans i in
													let c2 = nth !inv i in
													ans := changeithelement !ans i (dividel c1 (nth c1 i)) 0;
													inv := changeithelement !inv i (dividel c2 (nth c1 i)) 0;
													let c11 = nth !ans i in
													let c22 = nth !inv i in
													let l = mkcolumunit !ans i i 0 c11 !inv c22 in
													ans := get1 l;
													inv := get2 l;
													printm !ans;
													print_string ("now the other \n");
													printm !inv;
													print_string ("main \n");
													temp !ans (i+1) inv
					in temp m 0 inve
				else raise SingularMatrix
			else raise IncompatibleMatrixShape
	 	else raise IncompatibleMatrixShape 
	else [];;


let split3 m i= 
	let x = transm m in 
	let rec temp l c =
		match l with
		| [] -> []
		| h::t -> if c = i then t else
				  h::(temp t (c+1))
		in 
		transm (temp x 0);;


let crossprodv (v1:vector list):vector=
	let c = len v1 + 1 in
	let m = (v1:matrix) in
	if checkm m (c-1) c then
			let rec temp m i =
				if i = c then []
				else let t = split3 m i in
					printm t;
					 (-1.0**(float_of_int i))*.(detm t)::temp m (i+1)
			in temp m 0
	else raise UnequalVectorSize;;