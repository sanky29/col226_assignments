edge(a,b).
edge(b,c).
edge(b,d).
edge(d,e).
path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,W),path(W,Y). 

