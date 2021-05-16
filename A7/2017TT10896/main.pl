% defining the integers and boolean
num(X) :- integer(X).
exp(num(E)) :- num(E).
variable(X) :- string(X).
typeofvar(y,tint).

%variable abs
exp(variable(X)) :- variable(X).

%funcition definations
exp(funct(X,E)) :- variable(X),exp(E).
exp(app(funct(X,F),E)) :- exp(E),exp(F),variable(X).

% integer expressions
exp(add(E1,E2)) :- exp(E1),exp(E2).
exp(mul(E1,E2)) :- exp(E1),exp(E2).
exp(sub(E1,E2)) :- exp(E1),exp(E2).
exp(div(E1,E2)) :- exp(E1),exp(E2).
exp(mod(E1,E2)) :- exp(E1),exp(E2).
exp(abs(E1)) :- exp(E1).

% the boolean expressions
exp(t).
exp(f).
exp(or(E1,E2)) :- exp(E1),exp(E2).
exp(and(E1,E2)) :- exp(E1),exp(E2).
exp(not(E1)) :- exp(E1).

%n-tuples
exp([H|T]) :- exp(H),exp(T).
exp([]).

% comparision operators
exp(grt(E1,E2)) :- exp(E1),exp(E2).
exp(sml(E1,E2)) :- exp(E1),exp(E2).
exp(equal(E1,E2)) :- exp(E1),exp(E2).

% if_then_else
exp(if_then_else(E,E1,E2)) :- exp(E),exp(E1),exp(E2).

% the definations
exp(let_d_in_e(D,E)) :- def(D),exp(E).
def(assign(X,E)) :- variable(X),exp(E).
def(parallel(D1,D2)) :- def(D1),def(D2).
def(seq(D1,D2)) :- def(D1),def(D2).
def(let_d1_in_d2(D1,D2)) :- def(D1),def(D2).

%projection
exp(proj1(E1,E2)) :- exp(E1),exp(E2).
exp(proj2(E1,E2)) :- exp(E1),exp(E2).

%hastype functions
%vriable
hastype([(A,B)|R],variable(X),T) :- A=X ->(variable(X),(B=T)); hastype(R,variable(X),T).

%number
hastype(G,num(X),T) :- num(X),T=tint.
hastype(G,add(E1,E2),T) :- exp(add(E1,E2)),hastype(G,E1,tint),hastype(G,E2,tint),T=tint.
hastype(G,sub(E1,E2),T) :- exp(sub(E1,E2)),hastype(G,E1,tint),hastype(G,E2,tint),T=tint.
hastype(G,mul(E1,E2),T) :- exp(mul(E1,E2)),hastype(G,E1,tint),hastype(G,E2,tint),T=tint.
hastype(G,div(E1,E2),T) :- exp(div(E1,E2)),hastype(G,E1,tint),hastype(G,E2,tint),T=tint.
hastype(G,mod(E1,E2),T) :- exp(mod(E1,E2)),hastype(G,E1,tint),hastype(G,E2,tint),T=tint.
hastype(G,abs(E),T) :- exp(abs(E)),hastype(G,E,tint),T=tint.

%boolean
hastype(G,t,T) :- T=tbool.
hastype(G,f,T) :- T=tbool.
hastype(G,or(E1,E2),T) :- exp(or(E1,E2)),hastype(G,E1,tbool),hastype(G,E2,tbool),T=tbool.
hastype(G,and(E1,E2),T) :- exp(and(E1,E2)),hastype(G,E1,tbool),hastype(G,E2,tbool),T=tbool.
hastype(G,not(E),T) :- exp(E),hastype(G,E,tbool),T=tbool.

%tuple
hastype(G,[H|T1],T) :- hastype(G,H,T2),
					  (is_list(H) -> 
					  ( T1=[] -> (atomic_concat('( ',T2,T3),atomic_concat(T3,' )',T));
					  (hastype(G,T1,T3),atomic_concat('(',T2,T21),atomic_concat(T21,') * ',T5),atomic_concat(T5,T3,T)));
					  ( T1=[] -> T=T2;
					  (hastype(G,T1,T3),atomic_concat(T2,' * ',T5),atomic_concat(T5,T3,T)))).

%comparision
hastype(G,grt(E1,E2),T) :- exp(grt(E1,E2)),hastype(G,E1,tint),hastype(G,E2,tint),T=tbool.
hastype(G,sml(E1,E2),T) :- exp(sml(E1,E2)),hastype(G,E1,tint),hastype(G,E2,tint),T=tbool.
hastype(G,equal(E1,E2),T) :- exp(equal(E1,E2)),hastype(G,E1,T1),hastype(G,E2,T1),T=tbool.

%if_then_else
hastype(G,if_then_else(E,E1,E2),T) :- exp(if_then_else(E,E1,E2)),hastype(G,E,tbool),hastype(G,E1,T1),hastype(G,E2,T1),(T=T1).

%definations
typeElaborate(G,assign(X,E),G1) :- exp(E),variable(X),hastype(G,E,T),(G1 = [(X,T)]).
typeElaborate(G,parallel(D1,D2),G1) :- def(D1),def(D2),typeElaborate(G,D1,G2),typeElaborate(G,D2,G3),(append(G3,G2,G1)).
typeElaborate(G,seq(D1,D2),G1) :- def(D1),def(D2),typeElaborate(G,D1,G2),append(G2,G,G21),typeElaborate(G21,D2,G3),append(G3,G2,G1).	
typeElaborate(G,let_d1_in_d2(D1,D2),G1) :- def(D1),def(D2),typeElaborate(G,D1,G2),append(G2,G,G21),typeElaborate(G21,D2,G3),(G1 = G3).

%the assignment
hastype(G,let_d_in_e(D,E),T) :- def(D),exp(E),typeElaborate(G,D,G1),append(G,G1,G2),hastype(G2,E,T).

%functions 
hastype(G,funct(X,E),T) :- infer(E,X,T1),hastype([(X,T1)|G],E,T2),T=arrowtype(T1,T2).
hastype(G,app(funct(X,E1),E2),T) :- exp(funct(X,E1)),exp(E2),infer(E1,X,T1),hastype(G,E2,T1),hastype([(X,T1)|G],E1,T).

%projection 
hastype(G,proj1(E1,E2),T) :- exp(E1),exp(E2),hastype(G,E1,T).
hastype(G,proj2(E1,E2),T) :- exp(E1),exp(E2),hastype(G,E2,T).

%the infer function
infer(add(E1,E2),X,T) :- (E1=variable(X)->T=tint;(E2= variable(X) -> T=tint;(infer(E1,X,T1) ->T=T1; infer(E2,X,T)))).
infer(mul(E1,E2),X,T) :- (E1=variable(X)->T=tint;(E2= variable(X) -> T=tint;(infer(E1,X,T1) ->T=T1; infer(E2,X,T)))).
infer(div(E1,E2),X,T) :- (E1=variable(X)->T=tint;(E2= variable(X) -> T=tint;(infer(E1,X,T1) ->T=T1; infer(E2,X,T)))).
infer(sub(E1,E2),X,T) :- (E1=variable(X)->T=tint;(E2= variable(X) -> T=tint;(infer(E1,X,T1) ->T=T1; infer(E2,X,T)))).
infer(mod(E1,E2),X,T) :- (E1=variable(X)->T=tint;(E2= variable(X) -> T=tint;(infer(E1,X,T1) ->T=T1; infer(E2,X,T)))).
infer(abs(E),X,T) :- (E=variable(X)->T=tint;infer(E2,X,T)).
