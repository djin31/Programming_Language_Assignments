#use "2016CS10312.ml" 
let signat = [(Sym("0"),0);(Sym("1"),0); (Sym "double",1); (Sym("+"),2)];;

(* checking return_arity function *)
print_string "checking return_arity function ";;
return_arity signat (Sym ("+"));;
return_arity signat (Sym ("0"));;


let t1 = Node(Sym("+"),[Node(Sym("0"),[Node(Sym("0"),[])]);Node(Sym("1"),[])]);; 	(*incorrect term*)
let t2 = Node(Sym("+"),[Node(Sym("0"),[]);Node(Sym("1"),[])]);;						(*correct term*)

(*checking wf term*)
print_string "checking wf term ";;
wfterm signat t1;;
wfterm signat t2;; 

(* checking ht,size and vars *)
print_string "checking ht,size and vars ";;
ht t2;;
size t2;;
vars t2;;

(* terms with variables *)
print_string "terms with variables ";;
let t3 = Node(Sym("+"),[V(Var("x"));Node(Sym("1"),[])]);;
wfterm signat t3;;
ht t3;;
size t3;;
vars t3;; 

let t4 = Node(Sym("+"),[V(Var("x"));Node(Sym("double"),[V(Var "y")])]);;	(* t4 is x + 2*y *)
wfterm signat t4;;
ht t4;;
size t4;;
vars t4;; 

(* some substitution definitions *)
let sub1 = [(V(Var "x"), V(Var "c")); (V(Var "y"),Node(Sym("+"),[V(Var("c"));Node(Sym("1"),[])]))];; (* Var x -> Var c ; Var y -> Var c + Sym 1 *)
let sub2 = [(V(Var "x"), V(Var "p")); (V(Var "c"),t2)];;

(* checking subst function *)
print_string "checking subst function ";;
subst sub1 t3;;
subst sub1 t4;;

(* checking give_sub *)
print_string "checking give_sub ";;
give_sub sub1 (V(Var "m"),V(Var "y"));;  (* returns a substitution such that m -> substitution of y in sub1 *)

(* checking composition of substitutions *)
print_string "checking composition of substitutions ";;
let sub3 = compose sub1 sub2;;
let sub4 = compose sub2 sub1;;

print_string "checking substitution with composition of substitutions ";;
subst (compose sub1 sub2) t4;;

(* checking mgu *)
print_string "checking mgu with elementary substitutions ";;
mgu t3 (V(Var "y"));;
mgu (V(Var "y")) (V(Var "x"));;
mgu (V(Var "x")) (Node(Sym "0",[]));;

print_string "checking mgu with complex substitutions";;
let t5 = Node(Sym "+",[V(Var "p");Node(Sym "double",[Node(Sym "double",[V(Var "z")])])]);; (* t5 is plus(p,double(double(y)))*)
mgu t4 t5 ;;				(* t4 is plus(x,double(y)) *)
mgu (Node(Sym "+",[V(Var "x");V(Var "y")])) (Node(Sym "+",[V(Var "z");V(Var "x")]));;
mgu (Node(Sym "+",[V(Var "x");Node(Sym "0",[])])) (Node(Sym "+",[Node(Sym "0",[]);V(Var "x")]));;

(*Non unifiable cases*)
(*comment all preceding non unifiable cases to check*)
mgu (Node(Sym "+",[V(Var "x");Node(Sym "0",[])])) (Node(Sym "+",[Node(Sym "1",[]);V(Var "x")]));;
mgu (V(Var "x")) t3;;
mgu (Node(Sym "+",[V(Var "z");V(Var "y")])) (Node(Sym "*",[V(Var "y");V(Var "x")]));;
