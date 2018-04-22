
edge(a,b).
edge(b,c).
path(X,X).
path(X,Y):-edge(X,Z),path(Z,Y).