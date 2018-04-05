(* defining tables *)
let table1 = [Var "x",Clos([],Nat 20)];;
let table2 = [(Var "x", Clos([],Nat 1)); (Var "y", Clos([],Nat (-1))); (Var "b", Clos([],Bool true))];;

print_string "\ntesting basic operations of language";;
(*testing if else then conditional*)
print_string "\ntesting if else then conditional";;
let code = Add(Cond(Less(Nat 31, Nat 20),Nat 3,Nat 1),Nat 20);;
runKrivine [] code;;

(*testing binary arithmetic operands*)
print_string "\ntesting binary arithmetic operations";;
let code = Add(Nat 1, Mul(Nat 2, Nat 3));;
runKrivine [] code;;

(*testing boolean operation*)
print_string "\ntesting boolean operations";;
let code = And(Less(Nat 1,Nat 2),Equals(Add (Nat 1,Nat 1),Nat 2));;
runKrivine [] code;;

(*testing tuples*)
print_string "\ntesting tuples and projection";;
let x = Add(Nat 1, Proj (2,Tup([Nat 1; Nat 2; Nat 3])));;
runKrivine table1 x;;

let y = Tup([Nat 1; Nat 2; Nat 3]);;
runKrivine table1 y;;

let z = Equals(Tup([Nat 1; Nat 2; Nat 3]),Tup([Nat 1; Add(Nat 1,Nat 1); Nat 3]));;
runKrivine table1 z;;

(*testing lambda calculus*)
print_string "\n testing lambda calculus now";;

(*testing functions on variables*)
print_string "\ntesting functions on variables";;
let code = Add(Nat 1, Mul(Nat 2,V (Var "x")));;
runKrivine table2 code;;

let code = Add(Cond(Less(Nat 31, Nat 20),Nat 3,V(Var "x")),V(Var "y"));;
runKrivine table2 code;;

let code = Equals(Tup([Nat 1; Nat 2; Nat 3]),Tup([Nat 1; Add(V(Var "x"),Nat 1); Nat 3]));;
runKrivine table1 code;;
runKrivine table2 code;;

let code = Add(Nat 1, Proj (2,Tup([Nat 1; Nat 2; V(Var "x")])));;
runKrivine table1 code;;

(* defining abstractions *)
print_string "\n defining abstractions";;
let succ = Lambda(Var "x", Add(V (Var "x"), Nat 1));;
runKrivine table1 succ;;
runKrivine [] succ;;

let is_negative = Lambda(Var "x", Cond(Less (V (Var "x"),Nat 0), Bool true, Bool false));;
runKrivine table1 is_negative;;
runKrivine [] is_negative;;

print_string "\n testing invokation of abstractions";;
let code = (Call(succ,(V (Var "x"))));;
runKrivine table1 code;;

let code = Call(is_negative,Call(succ,V (Var "y")));; 
runKrivine table2 code;;

let code = Add(Nat 31,Cond(Call(is_negative,Call(succ,V (Var "y"))),Nat 2, Proj(1,Tup([V(Var "x");V(Var "y")]))));;
runKrivine table2 code;;

let code = Equals(V(Var "b"),Call(is_negative,Call(succ,V (Var "y"))));;
runKrivine table2 code;;

