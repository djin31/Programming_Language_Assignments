exception MachineStuck;;
exception UndefinedVariable;;
exception CompilationError;;

open List;;

type variable = Var of string;;

type exp =	 V of variable						(* variable definition *)
			|Lambda of variable*exp				(* abstraction definition *)
			|Call of exp*exp 					(* function call definition *)
			|Nat of int
			|Bool of bool		
			|Add of exp*exp						(* Binary operations *)
			|Mul of exp*exp							
			|And of exp*exp
			|Equals of exp*exp					(* comparisons *)
			|Less of exp*exp
			|Cond of exp*exp*exp				(* If then else conditional *)	
			|Tup of exp list 					(* Tuples *)
			|Proj of int*exp;;

type opcode = 	 VAR of string
				|LAMBDA of (opcode*(opcode list))
				|ARG of opcode list
				|APPLY
				|BOOL of bool
				|NUM of int
				|ADD
				|MUL
				|AND
				|EQUALS
				|LESS
				|COND of (opcode list)*(opcode list)
				|TUPLE of int;;
				

let rec map f l =match l with				(* implements higher order function map *)
				 [] -> []
				| x::xs -> (f x)::(map f xs);; 

let append l1 l2 =  l1 @ l2;;				

let rec reduce f l e = match l with 		(* implements higher order function reduce *)
						 [] -> e
						|x::xs -> reduce f xs (f e x);;

let rec pop n s e = if n=0 then e else (pop (n-1) (tl s) ([hd s]@e));;

let rec remove n s = if n=0 then s else (remove (n-1) (tl s));; 

let rec givepos n l = if n=0 then (hd l) else (givepos (n-1) (tl l));;

let rec map f l =match l with
				|[] -> []
				| x::xs -> (f x)::(map f xs);; 


let rec compile e = match e with
				|Nat (n) -> [NUM (n)]
				|Bool (b) -> [BOOL b]
				|Add(e1,e2) -> (compile e1)@(compile e2)@[ADD]
				|Mul(e1,e2) -> (compile e1)@(compile e2)@[MUL]
				|And(e1,e2) -> (compile e1)@(compile e2)@[AND]
				|Equals(e1,e2) -> (compile e1)@(compile e2)@[EQUALS]
				|Less(e1,e2) -> (compile e1)@(compile e2)@[LESS]
				|Cond(e0,e1,e2) -> (compile e0) @ [COND(compile e1, compile e2)]
				|Tup(l) -> (reduce append (map compile l) []) @ [TUPLE (length l)]			(* compiles the opcodes for all the tuple elements and concatenates them one after another *)
				|Proj(n,Tup(l)) -> compile(givepos n l)
				|V(Var x) -> [VAR x]
				|Lambda(Var x,e1) -> [LAMBDA(VAR x,(compile e1))]
				|Call(e1,e2) -> (compile e1)@[ARG(compile e2)]@[APPLY]
				|_ -> raise CompilationError;;

let rec lookup x table = match table with
							|[] -> raise UndefinedVariable
							|(y,z)::xs -> if y=x then z else (lookup x xs);;

type answer =	  Number of int 						(* mutually recursive definitions of table and answer so as to allow value closures *)
				| Boolean of bool
				| Tuple of answer list
				| Vclosure of table*variable*(opcode list)
and table = (variable*answer) list;;

let rec executeSECD stack env code dump = match (stack,env,code,dump) with
				|([v],e,[],[]) -> v
				|([v],e,[],(s,e',c)::d) -> executeSECD (v::s) e' c d
				|(s, e, (NUM n)::c, d) -> executeSECD ((Number n)::s) e c d
				|(s, e, (BOOL b)::c, d) -> executeSECD ((Boolean b)::s) e c d
				|((Number n1)::(Number n2)::s, e, ADD::c, d) -> executeSECD ((Number (n1+n2))::s) e c d
				|((Number n1)::(Number n2)::s, e, MUL::c, d) -> executeSECD ((Number (n1*n2))::s) e c d
				|((Boolean b1)::(Boolean b2)::s, e, AND::c, d) -> executeSECD ((Boolean (b1 && b2))::s) e c d
				|(v1::v2::s, e, EQUALS::c, d) -> if (v1=v2) then executeSECD ((Boolean true)::s) e c d
															else executeSECD ((Boolean false)::s) e c d
				|((Number n1)::(Number n2)::s, e, LESS::c, d) -> if (n2<n1) then executeSECD ((Boolean true)::s) e c d
																			else executeSECD ((Boolean false)::s) e c d															
				|((Boolean true)::s, e, COND(c1,c2)::c, d) -> executeSECD s e (c1@c) d
				|((Boolean false)::s, e, COND(c1,c2)::c, d) -> executeSECD s e (c2@c) d
				|(s, e, TUPLE(n)::c, d) -> executeSECD ((Tuple(pop n s []))::(remove n s)) e c d
				|(s, e, (VAR x)::c, d) -> executeSECD ((lookup (Var x) e)::s) e c d
				|(s, e, (LAMBDA(VAR x, t))::c, d) -> executeSECD ((Vclosure (e,Var x, t))::s) e c d
				|(s, e, ARG(l)::c, d) -> executeSECD s e (l@c) d
				|(arg::Vclosure(e',param,t)::s, e, APPLY::c, d) -> executeSECD [] ((param,arg)::e') t ((s,e,c)::d)
				|_ -> raise MachineStuck;;

let runSECD table e = executeSECD [] table (compile e) [];;

							 