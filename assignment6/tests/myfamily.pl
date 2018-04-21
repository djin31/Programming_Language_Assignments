male("R L Jindal").
male("J B Jindal").
male("Anil Jindal").
male("Jitendar Jindal").
male("Manoj Jindal").
male("Deepasnhu Jindal").

female("Krishna").
female("Anita").
female("Ritika").

equals(X,X).

notequals(A,B):-
	\+ equals(A,B).

couple("R L Jindal", "Krishna").
couple("J B Jindal","Anita").

married(A,B):-couple(A,B).
married(A,B):-couple(B,A).

father("R L Jindal", "J B Jindal").
father("R L Jindal", "Jitendar Jindal").
father("R L Jindal", "Anil Jindal").
father("R L Jindal", "Manoj Jindal").

mother(A,B):- father(C,B),married(A,C).
son(A,B):-father(B,A).
son(A,B):-mother(B,A).
brother(A,B):-mother(C,A),mother(C,B),male(A),notequals(A,B).
