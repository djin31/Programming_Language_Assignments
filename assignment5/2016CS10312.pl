%Lookup function to search the table
lookup(_,[],_):-fail.
lookup(X,[(X,Z)|_],Z):-!.
lookup(X,[(_,_)|Xs],W):-lookup(X,Xs,W).

%apppend
append([],L,L).
append([X|Xs],L2,[X|L3]):- append(Xs,L2,L3).

%FindPair and IntersectionCheck
findPair([(X,_)|_],X):-!.
findPair([(_,_)|Xs],X):-findPair(Xs,X).

empty_intersection(List1, List2) :-
	\+ (findPair(List1,Element), findPair( List2,Element)).


%Type axioms
istype(typeVar(_)).
istype(intT).
istype(boolT).

%Higher order types
istype(arrowT(T1,T2)):-istype(T1), istype(T2).
istype(tupType([])).
istype(tupType([X|Xs])):-istype(X), istype(Xs).

%Basic axioms 
hastype(_,X,intT):-integer(X).
hastype(_,X,boolT):-X == true.
hastype(_,X,boolT):-X == false.
hastype(Gamma,variable(X),T):- lookup(variable(X),Gamma,T).


%Derived expressions
%Arithmetic operations
hastype(Gamma,add(X,Y),intT):- hastype(Gamma,X,intT), hastype(Gamma,Y,intT).
hastype(Gamma,sub(X,Y),intT):- hastype(Gamma,X,intT), hastype(Gamma,Y,intT).
hastype(Gamma,mul(X,Y),intT):- hastype(Gamma,X,intT), hastype(Gamma,Y,intT).
hastype(Gamma,div(X,Y),intT):- hastype(Gamma,X,intT), hastype(Gamma,Y,intT),Y\==0.

%Boolean operations
hastype(Gamma,and(X,Y),boolT):-hastype(Gamma,X,boolT), hastype(Gamma,Y,boolT).
hastype(Gamma,or(X,Y),boolT):-hastype(Gamma,X,boolT), hastype(Gamma,Y,boolT).

%Comparison
hastype(Gamma,greater(X,Y),boolT):- hastype(Gamma,X,intT), hastype(Gamma,Y,intT).
hastype(Gamma,less(X,Y),boolT):- hastype(Gamma,X,intT), hastype(Gamma,Y,intT).

%Equality
hastype(Gamma,equals(X,Y),boolT):- hastype(Gamma,X,T), hastype(Gamma,Y,T).

%IfElse
hastype(Gamma,if_else(X,Y,Z),T):- hastype(Gamma,X,boolT), hastype(Gamma,Y,T), hastype(Gamma,Z,T), istype(T).

%Definitions
hastype(Gamma,let_in_end(D,E),T):- typeElaborates(Gamma,D,Gamma_prime), append(Gamma_prime,Gamma,NewGamma), hastype(NewGamma,E,T).

%Lambda Calculus
hastype(Gamma,lambda(variable(X),E),arrowT(T1,T2)):-hastype([(variable(X),T1)|Gamma],E,T2), istype(T1).
hastype(Gamma,apply(E1,E2),T2):-hastype(Gamma,E1,arrowT(T1,T2)), hastype(Gamma,E2,T1).

%Tuples
hastype(_,tup([]),tupType([])).
hastype(Gamma,tup([X|Xs]),tupType([T|Ts])):-hastype(Gamma,X,T), hastype(Gamma,tup(Xs),tupType(Ts)).

%Projection
hastype(Gamma,proj(N,tup(L)),T):-nth0(N,L,X), hastype(Gamma,X,T).

typeElaborates(Gamma,def(variable(X),E),[(variable(X),T)]):-hastype(Gamma,E,T).
typeElaborates(Gamma,parallel(D1,D2),T):-typeElaborates(Gamma,D1,T1), typeElaborates(Gamma,D2,T2), append(T1,T2,T), empty_intersection(T1,T2).
typeElaborates(Gamma,sequential(D1,D2),T):-typeElaborates(Gamma,D1,T1), append(T1,Gamma,Gamma_prime), typeElaborates(Gamma_prime,D2,T2), append(T2,T1,T).
typeElaborates(Gamma,localdef(D1,D2),T):-typeElaborates(Gamma,D1,T1), append(T1,Gamma,Gamma_prime), typeElaborates(Gamma_prime,D2,T). 
