

(*let atom = Pred("id",[Var "X";Var "X"]);;

let myprog = [Fact atom];;

let prog_goal = Pred("id",[Var "Y"; Node ("Z",[Const "e"])]);;

 let tmp = top myprog prog_goal;;  *)

let fact1 = Fact(Pred("edge",([Const "a"; Const "b"])));;
let fact2 = Fact(Pred("edge",([Const "b"; Const "c"])));;
let fact3 = Fact(Pred("path",([Var "#b"; Var "#b"])));;
let fact4 = Fact(Pred("edge",([Const "c"; Const "d"])));;
let rule1 = Rule(Pred("path",([Var "#X";Var "#Y"])),[Pred("edge",([Var "#X";Var "#Z"]));Pred("Path",([Var "#Z";Var "#Y"]))]);;
(* let rule1 = Rule(Pred("path",([Var "#X";Var "#Y"])),[Pred("edge",([Var "#X";Var "#Y"]))]);;  *)

let myprog2 = [fact1;fact2;fact3;fact4;rule1];;
let prog_goal = Pred("path",([Var "X";Var "Y"]));;
let tmp = top myprog2 prog_goal;;
