(* defining tables *)
let table1 = [Var "x",Number (20)];;
let table2 = [(Var "x", Number (1)); (Var "y", Number (-1)); (Var "b", Boolean(true))];;

print_string "\ntesting basic operations of language";;
(*testing if else then conditional*)
print_string "\ntesting if else then conditional";;
let code = Add(Cond(Less(Nat 31, Nat 20),Nat 3,Nat 1),Nat 20);;		(*(if (31<20) then 2 else 1) + 20*)
compile code;;
runSECD [] code;;

(*testing binary arithmetic operands*)
print_string "\ntesting binary arithmetic operations";;
let code = Add(Nat 1, Mul(Nat 2, Nat 3));;							(*1+2*3*)
compile code;;
runSECD [] code;;

(*testing boolean operation*)
print_string "\ntesting boolean operations";;
let code = And(Less(Nat 1,Nat 2),Equals(Add (Nat 1,Nat 1),Nat 2));;	(*(1<2) && ((1+1)==2)*)
compile code;;
runSECD [] code;;

(*testing tuples*)
print_string "\ntesting tuples and projection";;
let x = Add(Nat 1, Proj (2,Tup([Nat 1; Nat 2; Nat 3])));;			(* 1 + 3rd element of tuple(1;2;3) *)
compile x;;
runSECD table1 x;;

let y = Tup([Nat 1; Nat 2; Nat 3]);;								(* tuple(1;2;3) *)
runSECD table1 y;;

let z = Equals(Tup([Nat 1; Nat 2; Nat 3]),Tup([Nat 1; Add(Nat 1,Nat 1); Nat 3]));; (* tuple equality check *)
compile z;;
runSECD table1 z;;

(*testing lambda calculus*)
print_string "\n testing lambda calculus now";;

(*testing functions on variables*)
print_string "\ntesting functions on variables";;
let code = Add(Nat 1, Mul(Nat 2,V (Var "x")));;						(* 1 + 2*x *)
compile code;;
runSECD table2 code;;

let code = Add(Cond(Less(Nat 31, Nat 20),Nat 3,V(Var "x")),V(Var "y"));;	(*(if (31<20) then 3 else x) + y*)
compile code;;
runSECD table2 code;;

let code = Equals(Tup([Nat 1; Nat 2; Nat 3]),Tup([Nat 1; Add(V(Var "x"),Nat 1); Nat 3]));;	(* tuple equality check with variables *)
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

let code = Add(Nat 31,Cond(Call(is_negative,Call(succ,V (Var "y"))),Nat 2, Proj(1,Tup([V(Var "x");V(Var "y")]))));; (* 31 + if (y<0) then 2 else 2nd element of (x,y)*)
compile code;;
runSECD table2 code;;


let code = Equals(V(Var "b"),Call(is_negative,Call(succ,V (Var "y"))));;
compile code;;
runSECD table2 code;;

(*General testing*)
let var1 = Var ("X");;
let var2 = Var ("Y");;
let var3 = Var ("Z");;
let var4 = Var ("W");;
let myTable = [ (var1, Number (25));  (var2, Boolean (true));  (var3, Tuple ([Number (4); Boolean (false); Number (0)]));  (var4, Number (2))];;

let exp1 = Add (Nat (4), Nat(5));;
runSECD [] exp1;;

let exp2 = Equals (exp1, Nat(9));;
runSECD [] exp2;;

let exp3 = Cond (exp2, Nat (1), Nat (0));;
runSECD [] exp3;;

let exp4 = Cond (Less (Nat (0), Nat (8)), Nat (89), Mul (V var1, V var1));;
runSECD [] exp4;;

let exp5 = Tup([Nat (8); Bool true; Mul (Nat (9), Nat (2))]);;
runSECD [] exp5;;

(* \x = x + 3 *)
let func1 = Lambda (Var ("x"), Add (V (Var ("x")), Nat (3)));;
runSECD [] func1;;

let call1 = Call (func1, Nat (92));; (* 92 + 3 *)
runSECD [] call1;;

let call2 = Call (func1, call1);;
runSECD [] call2;;

(* \x = x * X *)
let func2 = Lambda (Var ("x"), Mul (V (Var "x"), V var1));;
let call2 = Call (func2, Nat (12));; (* 25 * 12 *)
runSECD myTable call2;;

(* \x = x * x *)
let func3 = Lambda (Var ("x"), Mul (V (Var "x"), V (Var "x")));;
let call3 = Call (func3, Nat (14));;
runSECD myTable call3;;

let call4 = Call (func3, call2);;
runSECD myTable call4;;

let call5 = Call (func3, call3);;
runSECD myTable call5;;

(* Global var2 is true *)
let func4 = Lambda (var2, Cond (V var2, Nat (3), Nat (9)));;
let call6 = Call (func4, Bool false);;
runSECD myTable call6;;

(* \x = x + w, ~ x + 2*)
let func5 = Lambda (var1, Add (V var1, V var4));;
let call7 = Call (func5, Nat (9));;
runSECD myTable call7;;

let exp6 = Tup ([exp1;exp2;exp4;exp5]);;
runSECD myTable exp6;;

let exp7 = Proj (3, exp6);;
runSECD myTable exp7;;

let exp8 = Proj (2, exp7);;
runSECD myTable exp8;;
