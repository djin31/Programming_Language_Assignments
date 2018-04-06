exception MachineStuck;;
exception UndefinedVariable;;
exception CompilationError;;
exception IncorrectCall;;

open List;;

type variable = Var of string;;

type exp = 	 V of variable						(* variable definition *)
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


let rec lookup x table = match table with
			|[] -> raise UndefinedVariable
			|(y,z)::xs -> if y=x then z else (lookup x xs);;

let rec replace n l e = if n=0 then (e::(tl l)) else ((hd l)::(replace (n-1) (tl l) e));;

let rec givepos n l = if n=0 then (hd l) else (givepos (n-1) (tl l));;

type closure = Clos of clostable*(exp)|HALF|FULL|PENDING of (exp list)|ClosTuple of closure list
and clostable = (variable*closure) list;;

let packtable t e = Clos(t,e);;

let rec executeKrivine clos stack = match (clos,stack) with
			(*base cases*)
			|(Clos(gamma,Nat n),[]) -> clos
			|(Clos(gamma,Bool b),[]) -> clos
			|(Clos(gamma,Lambda(x,e)),[]) -> clos
			|(ClosTuple(l),[]) -> clos
			(*Lambda calculus*)
			|(Clos(gamma,V x),s) -> executeKrivine (lookup x gamma) s
			|(Clos(gamma,Call(e1,e2)),s) ->(match e1 with 
											|Lambda(x,e1') -> executeKrivine (Clos((x,Clos(gamma,e2))::gamma,e1')) s
											|_ -> raise IncorrectCall)
			(*Binary operations*)
			|(Clos(gamma,Add(e1,e2)),s) -> executeKrivine (Clos(gamma,e1)) (HALF::Clos(gamma,Add(e1,e2))::s)
			|(Clos(gamma1,Nat n1),(HALF::Clos(gamma,Add(e1,e2))::s)) -> executeKrivine (Clos(gamma,e2)) (FULL::Clos(gamma,Add(Nat n1,e2))::s)
			|(Clos(gamma2,Nat n2),(FULL::Clos(gamma,Add(Nat n1,e2))::s)) -> executeKrivine (Clos(gamma,Nat (n1+n2))) s
			|(Clos(gamma,Mul(e1,e2)),s) -> executeKrivine (Clos(gamma,e1)) (HALF::Clos(gamma,Mul(e1,e2))::s)
			|(Clos(gamma1,Nat n1),(HALF::Clos(gamma,Mul(e1,e2))::s)) -> executeKrivine (Clos(gamma,e2)) (FULL::Clos(gamma,Mul(Nat n1,e2))::s)
			|(Clos(gamma2,Nat n2),(FULL::Clos(gamma,Mul(Nat n1,e2))::s)) -> executeKrivine (Clos(gamma,Nat (n1*n2))) s
			|(Clos(gamma,And(e1,e2)),s) -> executeKrivine (Clos(gamma,e1)) (HALF::Clos(gamma,And(e1,e2))::s)
			|(Clos(gamma1,Bool b1),(HALF::Clos(gamma,And(e1,e2))::s)) -> executeKrivine (Clos(gamma,e2)) (FULL::Clos(gamma,And(Bool b1,e2))::s)
			|(Clos(gamma2,Bool b2),(FULL::Clos(gamma,And(Bool b1,e2))::s)) -> executeKrivine (Clos(gamma,Bool (b1 && b2))) s 
			|(Clos(gamma,Equals(e1,e2)),s) -> let res1 = (executeKrivine (Clos(gamma,e1)) []) in let res2 = (executeKrivine (Clos(gamma,e2)) []) in executeKrivine (Clos(gamma,Bool (res1=res2))) s 
			|(Clos(gamma,Less(e1,e2)),s) -> executeKrivine (Clos(gamma,e1)) (HALF::Clos(gamma,Less(e1,e2))::s)
			|(Clos(gamma1,Nat n1),(HALF::Clos(gamma,Less(e1,e2))::s)) -> executeKrivine (Clos(gamma,e2)) (FULL::Clos(gamma,Less(Nat n1,e2))::s)
			|(Clos(gamma2,Nat n2),(FULL::Clos(gamma,Less(Nat n1,e2))::s)) -> executeKrivine (Clos(gamma,Bool (n1<n2))) s
			(*Conditional*)
			|(Clos(gamma,Cond(e0,e1,e2)),s) -> executeKrivine (Clos(gamma,e0)) (HALF::Clos(gamma,Cond(e0,e1,e2))::s)
			|(Clos(gamma1,Bool true),HALF::Clos(gamma,Cond(e0,e1,e2))::s) -> executeKrivine (Clos(gamma,e1)) s
			|(Clos(gamma1,Bool false),HALF::Clos(gamma,Cond(e0,e1,e2))::s) -> executeKrivine (Clos(gamma,e2)) s
			(*Tuples*)
			|(Clos(gamma,Tup(l)),s) -> executeKrivine (Clos(gamma,(hd l))) (PENDING ([])::Clos(gamma,Tup(tl l))::s)
			|(Clos(gamma1,Nat n),PENDING(ld)::Clos(gamma,Tup([]))::s) -> executeKrivine (ClosTuple(map (packtable gamma) (ld@[Nat n]))) s
			|(Clos(gamma1,Bool n),PENDING(ld)::Clos(gamma,Tup([]))::s) -> executeKrivine  (ClosTuple(map (packtable gamma) (ld@[Bool n]))) s
			|(Clos(gamma1,Lambda (x,e)),PENDING(ld)::Clos(gamma,Tup([]))::s) -> executeKrivine  (ClosTuple(map (packtable gamma) (ld@[Lambda (x,e)]))) s
			(* |(Clos(gamma1,Tup n),PENDING(ld)::Clos(gamma,Tup([]))::s) -> executeKrivine  (ClosTuple(map (packtable gamma) (ld@[Tup n]))) s *)
			|(Clos(gamma1,Nat n),PENDING(ld)::Clos(gamma,Tup(lp))::s) -> executeKrivine (Clos(gamma,(hd lp))) (PENDING(ld@[Nat n])::Clos(gamma,Tup(tl lp))::s)
			|(Clos(gamma1,Bool n),PENDING(ld)::Clos(gamma,Tup(lp))::s) -> executeKrivine (Clos(gamma,(hd lp))) (PENDING(ld@[Bool n])::Clos(gamma,Tup(tl lp))::s)
			|(Clos(gamma1,Lambda (x,e)),PENDING(ld)::Clos(gamma,Tup(lp))::s) -> executeKrivine (Clos(gamma,(hd lp))) (PENDING(ld@[Lambda (x,e)])::Clos(gamma,Tup(tl lp))::s)
			(* |(Clos(gamma1,Tup n),PENDING(ld)::Clos(gamma,Tup(lp))::s) -> executeKrivine (Clos(gamma,(hd lp))) (PENDING(ld@[Tup n])::Clos(gamma,Tup(tl lp))::s) *)
			|(Clos(gamma,Proj(n,Tup(l))),s) -> executeKrivine (Clos(gamma,(givepos n l))) s;;

			

type answer =	  Number of int 						(* mutually recursive definitions of table and answer so as to allow value closures *)
				| Boolean of bool
				| Tuple of answer list
				| Vclosure of clostable*variable*(exp);;
let rec unpack clos = match clos with 
			|Clos(gamma,Nat n) -> Number n
			|Clos(gamma, Bool b) -> Boolean b
			|ClosTuple(l) -> Tuple(map unpack l)
			|Clos(gamma,Lambda (x,e)) -> Vclosure (gamma,x,e);;

let  runKrivine t e = unpack(executeKrivine (Clos(t,e)) []) ;;


