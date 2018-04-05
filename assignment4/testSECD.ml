(* defining tables *)
let table1 = [Var "x",Number (20)];;
let table2 = [(Var "x", Number (1)); (Var "y", Number (-1)); (Var "b", Boolean(true))];;

print_string "\ntesting basic operations of language";;
(*testing if else then conditional*)
print_string "\ntesting if else then conditional";;
let code = Add(Cond(Less(Nat 31, Nat 20),Nat 3,Nat 1),Nat 20);;
compile code;;
runSECD [] code;;

(*testing binary arithmetic operands*)
print_string "\ntesting binary arithmetic operations";;
let code = Add(Nat 1, Mul(Nat 2, Nat 3));;
compile code;;
runSECD [] code;;

(*testing boolean operation*)
print_string "\ntesting boolean operations";;
let code = And(Less(Nat 1,Nat 2),Equals(Add (Nat 1,Nat 1),Nat 2));;
compile code;;
runSECD [] code;;

(*testing tuples*)
print_string "\ntesting tuples and projection";;
let x = Add(Nat 1, Proj (2,Tup([Nat 1; Nat 2; Nat 3])));;
compile x;;
runSECD table1 x;;

let y = Tup([Nat 1; Nat 2; Nat 3]);;
runSECD table1 y;;

let z = Equals(Tup([Nat 1; Nat 2; Nat 3]),Tup([Nat 1; Add(Nat 1,Nat 1); Nat 3]));;
compile z;;
runSECD table1 z;;

(*testing lambda calculus*)
print_string "\n testing lambda calculus now";;

(*testing functions on variables*)
print_string "\ntesting functions on variables";;
let code = Add(Nat 1, Mul(Nat 2,V (Var "x")));;
compile code;;
runSECD table2 code;;

let code = Add(Cond(Less(Nat 31, Nat 20),Nat 3,V(Var "x")),V(Var "y"));;
compile code;;
runSECD table2 code;;

let code = Equals(Tup([Nat 1; Nat 2; Nat 3]),Tup([Nat 1; Add(V(Var "x"),Nat 1); Nat 3]));;
compile code;;
runSECD table1 code;;
runSECD table2 code;;

let code = Add(Nat 1, Proj (2,Tup([Nat 1; Nat 2; V(Var "x")])));;
compile code;;
runSECD table1 code;;

(* defining abstractions *)
print_string "\n defining abstractions";;
let succ = Lambda(Var "x", Add(V (Var "x"), Nat 1));;
compile succ;;
runSECD table1 succ;;
runSECD [] succ;;

let is_negative = Lambda(Var "x", Cond(Less (V (Var "x"),Nat 0), Bool true, Bool false));;
compile is_negative;;
runSECD table1 is_negative;;
runSECD [] is_negative;;

print_string "\n testing invokation of abstractions";;
let code = (Call(succ,(V (Var "x"))));;
runSECD table1 code;;

let code = Call(is_negative,Call(succ,V (Var "y")));; 
compile code;;
runSECD table2 code;;

let code = Add(Nat 31,Cond(Call(is_negative,Call(succ,V (Var "y"))),Nat 2, Proj(1,Tup([V(Var "x");V(Var "y")]))));;
compile code;;
runSECD table2 code;;


let code = Equals(V(Var "b"),Call(is_negative,Call(succ,V (Var "y"))));;
compile code;;
runSECD table2 code;;

