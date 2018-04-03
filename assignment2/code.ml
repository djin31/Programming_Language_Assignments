(*---------- Type Declarations ----------*)
type exp  =	 Const of int
			|T
			|F
			|Var of string
			|Abs of exp
			|Add of exp*exp
			|Sub of exp*exp
			|Exp of exp*exp
			|Mul of exp*exp
			|Div of exp*exp
			|Mod of exp*exp
			|Not of exp
			|And of exp*exp
			|Or of exp*exp
			|Implies of exp*exp
			|Eq of exp*exp
			|Gt of exp*exp
			|Lt of exp*exp
			|Ge of exp*exp
			|Le of exp*exp
			|Tup of exp list
			|Proj of exp*exp;;


type answer = Number of int 
			| Boolean of bool 
			| Tuple of answer list;;

type opcode  = 	 NUM of int
				|BOOL of bool
				|VAR of string
				|ABS
				|ADD
				|SUB
				|MUL
				|DIV
				|MOD
				|EXP
				|EQ
				|LE
				|LT
				|GE
				|GT
				|NOT
				|AND
				|OR
				|IMP
				|TUP of int
				|PROJ;;

(*-------- Exceptions ------------*)
exception NotCorrectType;;
exception TooShort;;


(*-------- Utility Helper Functions ----------*)
let absolute n = if (n<0) then (-n) else n;;

let rec pow a n = match  n with
				 0 -> 1
				| 1 -> a
				| n -> 
				let b = pow a (n / 2) in
				b * b * (if n mod 2 = 0 then 1 else a);;

let rec map f l =match l with
				 [] -> []
				| x::xs -> (f x)::(map f xs);; 

let rec reduce l e = match l with
					[]->e | x::xs -> reduce xs (x @ e);;

let rec givepos l i e = match l with
						 []-> raise TooShort
						|x::xs-> if (i=e) then x 
								 else (if (i>e) then (givepos xs i (e+1)) else raise TooShort);;

let rec givetop l n e retVal = if (e<n) then (givetop (List.tl l)  n (e+1) (retVal@[List.hd l])) else retVal;; 

let rec removetop l n e = if (e<n) then (removetop (List.tl l) n (e+1)) else l;;

(*------ A representative Table Function ---------*)
(*Modify here to change the assignment of variables*)
let table x = match x with
			 Var("x")->Number(31)
			|Var("y")->Number(20)
			|Var("b")->Boolean(true)
			|_-> Number(0);;


(*--------- Evaluate Functions -----------*)				
let rec evalnumber e = match e with
					 Const(n)->n
					|Var(s)->(match (table e) with
								Number(n)->n | _-> raise NotCorrectType)
					|Abs(e1)-> (absolute (evalnumber e1))
					|Add(e1,e2)-> (evalnumber e1) + (evalnumber e2)
					|Sub(e1,e2)-> (evalnumber e1) - (evalnumber e2)
					|Mul(e1,e2)-> (evalnumber e1) * (evalnumber e2)
					|Div(e1,e2)-> (evalnumber e1) / (evalnumber e2)
					|Mod(e1,e2)-> (evalnumber e1) mod (evalnumber e2)
					|Exp(e1,e2)-> (pow (evalnumber e1) (evalnumber e2))
					|_-> raise NotCorrectType;;

let rec evalbool e = match e with
					 T->true
					|F->false
					|Var(s)->(match (table e) with
								Boolean(b)->b | _-> raise NotCorrectType)
					|Not(e1)-> not(evalbool e1)
					|And(e1,e2)-> (evalbool e1) && (evalbool e2)
					|Or(e1,e2)-> (evalbool e1) || (evalbool e2)
					|Implies(e1,e2)-> (not(evalbool e1)) || ((evalbool e1)&&(evalbool e2))
					|_-> raise NotCorrectType;;


let rec eval e = match e with 
				 Const(n)->Number(n)
				|T->Boolean(true)
				|F->Boolean(false)
				|Var(s)-> table e
				|Abs(e) -> Number(evalnumber e)
				|Add(e1,e2) -> Number(evalnumber e)
				|Sub(e1,e2) -> Number(evalnumber e)
				|Mul(e1,e2) -> Number(evalnumber e)
				|Div(e1,e2) -> Number(evalnumber e)
				|Mod(e1,e2) -> Number(evalnumber e)
				|Exp(e1,e2) -> Number(evalnumber e)
				|Eq(e1,e2) -> if (evalnumber e1)=(evalnumber e2) then Boolean(true) else Boolean(false)
				|Le(e1,e2) -> if (evalnumber e1)<=(evalnumber e2) then Boolean(true) else Boolean(false)
				|Lt(e1,e2) -> if (evalnumber e1)<(evalnumber e2) then Boolean(true) else Boolean(false)
				|Ge(e1,e2) -> if (evalnumber e1)>=(evalnumber e2) then Boolean(true) else Boolean(false)
				|Gt(e1,e2) -> if (evalnumber e1)>(evalnumber e2) then Boolean(true) else Boolean(false)
				|Not(e1)-> Boolean(evalbool e1)
				|And(e1,e2)-> Boolean(evalbool e) 
				|Or(e1,e2)-> Boolean(evalbool e)
				|Implies(e1,e2)-> Boolean(evalbool e)
				|Tup(l)-> Tuple(map eval l)
				|Proj(e1,Tup(l))-> eval(givepos l (evalnumber e1) 0)
				|_-> raise NotCorrectType;;


(*--------- Compiler Function ----------*)
let rec compile e = match e with
				 Const(n) -> [NUM(n)]
				|T -> [BOOL(true)]
				|F -> [BOOL(false)]
				|Var(s) -> [VAR(s)]
				|Abs(e) -> (compile e)@[ABS] 
				|Add(e1,e2) -> (compile e2) @ (compile e1) @ [ADD]
				|Sub(e1,e2) -> (compile e2) @ (compile e1) @ [SUB]
				|Mul(e1,e2) -> (compile e2) @ (compile e1) @ [MUL]
				|Div(e1,e2) -> (compile e2) @ (compile e1) @ [DIV]
				|Exp(e1,e2) -> (compile e2) @ (compile e1) @ [EXP]
				|Mod(e1,e2) -> (compile e2) @ (compile e1) @ [MOD]
				|Eq(e1,e2) -> (compile e2) @ (compile e1) @ [EQ]
				|Le(e1,e2) -> (compile e2) @ (compile e1) @ [LE]
				|Lt(e1,e2) -> (compile e2) @ (compile e1) @ [LT]
				|Ge(e1,e2) -> (compile e2) @ (compile e1) @ [GE]
				|Gt(e1,e2) -> (compile e2) @ (compile e1) @ [GT]
				|Not(e) -> (compile e) @ [NOT] 
 				|And(e1,e2) -> (compile e2) @ (compile e1) @ [AND]
				|Or(e1,e2) -> (compile e2) @ (compile e1) @ [OR]
				|Implies(e1,e2) -> (compile e2) @ (compile e1) @ [IMP]
				|Tup(l)-> (reduce (map compile l) []) @ [TUP(List.length l)]
				|Proj(e1,Tup(l)) -> (compile e1) @ (compile (Tup(l))) @ [PROJ]
				|_-> raise NotCorrectType;;
			 	

(*------- Execute Machine ----------*)
let rec execute (s,rho,code) = match s,rho,code with
							 (s',rho,[])-> List.hd s
							|(s',rho,NUM(n)::c)-> execute(Number(n)::s',rho,c)
							|(s',rho,BOOL(b)::c)-> execute(Boolean(b)::s',rho,c)
							|(s',rho,VAR(st)::c)->execute((rho (Var(st)))::s',rho,c)
							|(Number(n)::s',rho,ABS::c)->execute((Number(absolute n))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,ADD::c)->execute((Number(n1+n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,SUB::c)->execute((Number(n1-n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,MUL::c)->execute((Number(n1*n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,DIV::c)->execute((Number(n1/n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,EXP::c)->execute((Number(pow n1 n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,MOD::c)->execute((Number(n1 mod n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,EQ::c)->execute((Boolean(n1=n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,LT::c)->execute((Boolean(n1<n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,LE::c)->execute((Boolean(n1<=n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,GT::c)->execute((Boolean(n1>n2))::s',rho,c)
							|(Number(n1)::Number(n2)::s',rho,GE::c)->execute((Boolean(n1>=n2))::s',rho,c)
							|(Boolean(b)::s',rho,NOT::c)->execute((Boolean(not b))::s',rho,c)
							|(Boolean(b1)::Boolean(b2)::s',rho,AND::c)->execute((Boolean(b1 && b2))::s',rho,c)
							|(Boolean(b1)::Boolean(b2)::s',rho,OR::c)->execute((Boolean(b1 || b2))::s',rho,c)
							|(Boolean(b1)::Boolean(b2)::s',rho,IMP::c)->execute((Boolean((not b1) || (b1 && b2)))::s',rho,c)
							|(s',rho,TUP(n)::c)->execute(Tuple(givetop s' n 0 [])::(removetop s' n 0),rho,c)
							|(Tuple(l)::Number(n)::s',rho,PROJ::c)->execute((givepos l n 0)::s',rho,c)
							|_->raise NotCorrectType;;


(*-------- Examples -----------*)
(*-------- Examples for calculation involving integers and tuple functions ----------*)
let x = Add(Add(Const(3),(Exp(Const(2),Const(3)))),Mul(Const(2),Const(1)));;		(* x = 3 + 2^3 + 2*1 *)
let y = Add(Mod(Const(31),Const(20)),Div(Const(31),Const(20)));;					(* y = 31%20 + 31/20 *)
let z = Add(x,y);;

let tup = Tup([x;y;z]);;
let proj = Proj(Const 1,tup);;														(* Projects y *)

let xcode = compile x;;
let ycode = compile y;;
let zcode = compile z;;
let tupcode = compile tup;;
let projcode = compile proj;;

let x1 = eval x;;
let y1 = eval y;;
let z1 = eval z;; 
let tupcode = eval tup;;

x1 = execute ([],table,xcode);;
y1 = execute ([],table,ycode);;
z1 = execute ([],table,zcode);;
y1 = execute ([],table,projcode);;

(*--------- Examples for calculation involving booleans --------*)
let bx = Or(And(T,F),T);;
let by =  Implies(bx,Or(F,F));;

let bxcode = compile bx;;
let bycode = compile by;;

eval bx = execute ([],table,bxcode);;
eval by = execute ([],table,bycode);;

(*------- Examples ---------*)
(* Example using a numeral variable *)
let x = Add(Var("x"),Mul(Var("y"),Const(3)));;				(* x = var(x) + 3*var(y) *)
eval x;;
eval x = execute ([],table,compile x);;

(* Example using a boolean variable *)
let index = Sub(Div(Add(Var("x"),Mul(Var("y"),Const(3))),Const(60)),Const(1));;		(* index = int(x-1) = int((var(x) + 3*var(y))/60 - 1) = int((31 + 20*3)/60 -1) = 0 *)
let y = Proj(index,Tup([Or(And(T,F),Var("b"));Div(Const(31),Const(20))]));;			(* tup = ((T & F) or Var(z), int(31/20) *) (*y = 0th index of it*)
eval y;;
eval y = execute ([],table,compile y);;