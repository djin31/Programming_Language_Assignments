%Constants
hastype([],31,T).
hastype([],true,T).

%arithmetic and boolean operations
hastype([],add(3,4),T).
hastype([],sub(add(3,4),5),T).
hastype([],and(equals(3,3),true),T).
hastype([],and(equals(3,false),true),T).
hastype([],less(2,3),T).

%equals
hastype([],equals(tup([3,true,tup([1])]),tup([31,true,tup([11])])),T).

%tuples and projections
hastype([],tup([3,true,add(3,4),equals(tup([2,3]),tup([3,4])),tup([1,2,3])]),T).
hastype([],proj(4,tup([3,true,add(3,4),equals(tup([2,3]),tup([3,4])),tup([1,2,3])])),T).
hastype([],proj(2,tup([3,true,add(3,4),equals(tup([2,3]),tup([3,4])),tup([1,2,3])])),T).

%lambda calculus
hastype([(variable("X"),boolT),(variable("Y"),intT)],variable("X"),T).
hastype([(variable("X"),boolT),(variable("Y"),intT)],add(add(variable("Y"),3),3),T).
hastype([(variable("X"),boolT),(variable("X"),intT)],variable("X"),T).

hastype([(variable("X"),boolT),(variable("Y"),intT)],lambda(variable("Y"),add(variable("Y"),3)),T).
hastype([(variable("X"),boolT),(variable("Y"),intT)],lambda(variable("X"),add(variable("Y"),3)),T).
hastype([(variable("X"),boolT),(variable("Y"),intT)],lambda(variable("X"),lambda(variable("Y"),add(variable("Y"),3))),T).
hastype([(variable("X"),boolT),(variable("Y"),intT)],lambda(variable("Z"),lambda(variable("Y"),add(variable("Y"),add(variable("Z"),3)))),T).
hastype([(variable("X"),boolT),(variable("Y"),intT)],lambda(variable("Z"),tup([variable("X"),variable("Y"),variable("Z")])),T).
hastype([(variable("X"),boolT),(variable("Y"),intT)],lambda(variable("X"),tup([variable("X")])),T).

hastype([(variable("X"),boolT),(variable("Y"),intT)],apply(lambda(variable("Y"),add(variable("Y"),3)),variable("Y")),T).
hastype([(variable("X"),boolT),(variable("Y"),intT)],apply(lambda(variable("X"),add(variable("Y"),3)),3),T).
hastype([(variable("X"),boolT),(variable("Y"),intT)],apply(lambda(variable("X"),add(variable("Y"),3)),true),T).

%if_else
hastype([(variable("X"),boolT),(variable("Y"),intT)],if_else(greater(variable("Y"),0),variable("Y"),0),T).

%definitions
hastype([(variable("Y"),intT)],let_in_end(def(variable("X"),3),add(variable("Y"),variable("X"))),T).
hastype([],let_in_end(def(variable("id"),lambda(variable("x"),variable("x"))),apply(id,3)),T).
typeElaborates([],def(variable("X"),add(3,4)),T).
typeElaborates([],def(variable("Y"),true),T).
typeElaborates([],parallel(def(variable("X"),3),def(variable("Y"),true)),T).
typeElaborates([],parallel(def(variable("X"),3),def(variable("X"),true)),T).
typeElaborates([],sequential(def(variable("X"),mul(31,20)),def(variable("Y"),true)),T).
typeElaborates([(variable("X"),boolT),(variable("Y"),intT)],
				localdef(
							def(variable("X"),31),
						 	parallel(def(variable("X"),tup([variable("Y")])),def(variable("Y"),false))

						 ),T).

typeElaborates([(variable("X"),boolT),(variable("Y"),intT)],
				localdef(
							def(variable("X"),20),
						 	parallel(def(variable("X"),3),def(variable("Y"),false))

						 ),T).


%example for the proof required
hastype([],proj(0,tup([1])),T).
hastype([],proj(0,tup([X])),T).
