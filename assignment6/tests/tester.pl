male(a).
male(b).
male(c).
female(d).
married(d,a).

son(b,a).
son(c,a).
son(X,Y) :- married(Y,Z),son(X,Z).