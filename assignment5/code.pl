%lookup function to search the table
lookup(X,[],Y):-fail.
lookup(X,[(X,Z)|Xs],Z):-!.
lookup(X,[(Y,Z)|Xs],W):-lookup(X,Xs,W).

%basic axioms 