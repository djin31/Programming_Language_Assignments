#use "code.ml";;

let atom = Pred("id",[Var "X";Var "X"]);;

let myprog = [Fact atom];;

let prog_goal = [Pred("id",[Var "Y"; Node ("Z",[Const "e"])])];;

let tmp = solve_goal myprog myprog prog_goal;;
