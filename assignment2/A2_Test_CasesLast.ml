(* Note:
 * Please make suitable changes to the shared test cases so that
 * the constructors match your signature definition.
 *)

#use "2016CS10312.ml";;
(*--==Compile & execute ==--*)
print_string "\nRunning the testcases\n\n";;

let rho x = match x with
			 Var("x")->Number(31)
			|Var("y")->Number(20)
			|Var("b")->Boolean(true)
			|_-> Number(0);;

eval (Add(Const(1),Const(2)));;
execute ([], rho, (compile (Add(Const(1),Const(2)))));;

eval (Mul(Const(6),Const(6)));;
execute ([], rho, (compile (Mul(Const(6),Const(6)))));;

eval (Exp(Const(2),Const(4)));;
execute ([], rho, (compile (Exp(Const(2),Const(4)))));;

eval (Div(Const(6),Const(3)));;
execute ([], rho, (compile (Div(Const(6),Const(3)))));;

eval (Var("x"));;
execute ([], rho, (compile (Var("x"))));;

eval (Var("y"));;
execute ([], rho, (compile (Var("y"))));;

eval (Abs(Const(-1)));;
execute ([], rho, (compile (Abs(Const(-1)))));;

eval (Proj(Const(2),Tup([(Const 12);(Const 121);(Const 33)])));;
execute ([], rho, (compile (Proj(Const 2,Tup([Const(12);Const(121);Const(33)])))));;

eval (And(T, F));;
execute ([],rho,compile (And(T, F)));;

execute ([], rho, (compile (Sub(Proj(Const(1),Tup([Const(2);Const(5);Const(8)])),Const(2)))));;
execute ([], rho, (compile (Sub(Proj(Const(1),Tup([Const(2);Const(5);Const(8)])),Const(2)))));;

eval (Implies(Not(Implies(Or((T), (F)), And((T), (F)))),Implies(And((T), (F)), Or((T), (F)))));;
execute ([],rho,compile (Implies(Not(Implies(Or((T), (F)), And((T), (F)))),Implies(And((T), (F)), Or((T), (F))))));;

eval (Ge(Const(4),Const(2)));;
execute ([],rho,compile (Ge(Const(4),Const(2))));;

eval (Le(Const(4),Const(2)));;
execute ([],rho,compile (Le(Const(4),Const(2))));;

let code = compile (Or(Eq(Const(5),Const(5)),And(Eq(Sub(Const(2),Const(1)),Const(1)),Mod(Proj(Const(2),Tup([Const(2);Const(5);Const(8)])),Const 2))));;
eval (Or(Eq(Const(5),Const(5)),And(Eq(Sub(Const(2),Const(1)),Const(1)),Mod(Proj(Const(2),Tup([Const(2);Const(5);Const(8)])),Const 2))));;
execute ([],rho,code);;

(*They were not asked to be implemented in assignment *)
(*
Ifthenelse(Gtr(Const(4),Const(2)),Add(Const(1),Const(3)),Sub(Const(1),Const(3)));

(* Lambda is a lambda function of type exp*exp and LetinEnd is a ternary operator of type exp*exp*exp *)
Apply(Lambda(Var("x"),LetinEnd(Para[Assgn(Var("a"),Const(2))],Add(Var("a"),Var("x")))),Const(2))

*)
