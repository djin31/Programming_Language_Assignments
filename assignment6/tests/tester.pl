id(X,X).
?- id(X).

edge(a,b).
edge(b,c).
path(X,Y):-edge(X,Y).