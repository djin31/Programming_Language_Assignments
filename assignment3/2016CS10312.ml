(* Exception Definitions *)
exception Invalid;;
exception NOT_UNIFIABLE;;

(* Type Definitions *)
type variable = Var of string;;
type symbol = Sym of string;;
type term = V of variable | Node of symbol * (term list);;
type signature = (symbol*int) list;;
type substitution = (term * term) list;; (* substitutions are represented as a list containing pair of variable and substituting *)

(* A representative signature definition *)
let signat = [(Sym("0"),0);(Sym("1"),0); (Sym "double",1); (Sym("+"),2)];;

(* Generic Helper Functions *)
(* fold function is a tail recursive function which takes seed value e and apply function f corecursively to elements of l *)
let rec fold f e l = match l with 
					 [] -> e
					|x::xs -> fold f (f x e) xs;;
(* map applies f to each element of l *)
let rec map f l = match l with 
					 [] -> []
					|x::xs -> (f x)::(map f xs);;

(* performs boolean and on operands a and b (required to apply fold to wfterm calculation) *)
let and_bool a b = a && b;;

let rec add x y = x + y;;

(* to check wether an element exists in a list *)
let rec find l e = match l with
					 [] -> false
					|x::xs -> if x = e then true else (find xs e);;  

(* to return union of two sets represented as list *)
let rec union l1 l2 = match l2 with
					 [] -> l1
					|x::xs -> if (find l1 x) then (union l1 xs) else (union (x::l1) xs);;

(* find if a pair with given first element exists *)
let rec find_pair pairlist v = match pairlist with 
						 [] -> false
						|(vx,t)::xs -> if (vx = v) then true else (find_pair xs v);;   

(* to return union based on keys of two list containing key value pair *)
let rec union_pair l1 l2 = match l2 with
					 [] -> l1
					|(v,t)::xs -> if (find_pair l1 v) then (union l1 xs) else (union ((v,t)::l1) xs);;

(* check signature function validates a signature by checking that it doesn't contain multiple definitions for a symbol and the arities are non negative *)
let rec check_sig sign = match sign with 
						 [] -> true
						|x::xs -> match x with 
								 (Sym s,n) -> if ((n<0) || (find_pair xs (Sym s))) then false else (check_sig xs) ;;

(* returns arity of symbol s in signature sign *)
let rec return_arity sign s = match sign with
							 [] -> raise Invalid
							|(sym,arity)::xs -> if sym = s then arity else (return_arity xs s) ;;

(* validates the pre term *)
let rec wfterm sign t = match t with
					 	 V var -> true
						|Node(s,l) -> if (List.length l) <> (return_arity sign s) then false
									  else (fold and_bool true (map (wfterm sign) l));;

(* finds height of a term tree *)
let rec ht t = match t with
				 V var -> 0
				|Node(Sym s,[]) -> 0
				|Node(Sym s,l) -> 1 + (fold max 0 (map ht l));;

(* finds size of a term tree *)
let rec size t = match t with
				 V var -> 1
				|Node(Sym s,[]) -> 1
				|Node(Sym s,l) -> 1 + (fold add 0 (map size l));;

(* finds variables of a term tree *)
let rec vars t = match t with
				 V var -> [var]
				|Node(Sym s,[]) -> []
				|Node(Sym s,l) -> (fold union [] (map vars l));;

(* return substituent value for a variable *)
let rec findsub s v = match s with
					 [] -> v
					|(vx , t)::xs -> if (vx = v) then t else (findsub xs v);;   

(* implements homomorphic extension of substitution *)
let rec subst sigma t = match t with
				 Node(Sym s,[]) -> (findsub sigma t)
				|V var ->  (findsub sigma t)
				|Node(Sym s,l) -> Node(Sym s,(map (subst sigma) l));; 

(* Representative substitution *)
let sub1 = [(V(Var "d"), V(Var "c"))];; 

(* composes a particular substitution pair with another substitution *)
let rec give_sub sigma sub = match sub with
						(t1,t2) -> (t1,subst sigma t2);;

(* composes two substitutions *)
let compose sig1 sig2 = union_pair (map (give_sub sig2) sig1) sig2;; 

(* implements fold operation on l1 l2 with a function which accepts two arguments *)
let rec unify s f l1 l2 = match (l1,l2) with
					 ([],[]) -> s
					|(x1::xs1,x2::xs2) -> let s' = (compose s (f x1 x2)) in (unify s' f (map (subst s') xs1) (map (subst s') xs2))
					|([],(x::xs)) -> raise NOT_UNIFIABLE
					|((x::xs),[]) -> raise NOT_UNIFIABLE;;

(* finds mgu for terms *)
let rec mgu t1 t2 = match (t1,t2) with
					 (V v1, V v2) -> if (v1=v2) then [] else [t1,t2]
					|(V v1, Node(Sym s,[])) -> [t1,t2]
					|(Node(Sym s,[]), V v2) -> [t2,t1] 
					|(V v1, Node(Sym s,l)) -> if (find l t1) then raise NOT_UNIFIABLE else [t1,t2]
					|(Node(Sym s,l), V v2) -> if (find l t2) then raise NOT_UNIFIABLE else [t2,t1]
					|(Node(Sym s1,[]),Node(Sym s2,[])) -> if (s1 = s2) then [] else raise NOT_UNIFIABLE
					|(Node(Sym s1,[]),Node(Sym s2,l)) -> raise NOT_UNIFIABLE
					|(Node(Sym s1,l),Node(Sym s2,[])) -> raise NOT_UNIFIABLE
					|(Node(Sym s1,l1),Node(Sym s2,l2)) -> if (s1 = s2) then (unify [] mgu l1 l2) else raise NOT_UNIFIABLE;;