exception NOT_UNIFIABLE;;
exception FAILURE;;

type term = 
      |Var of string
      |Const of string
      |Nat of int
      |T
      |F
      |Node of string*(term list);;

type atomic = Pred of string*(term list)|Cut;;

type clause =  
      |Fact of atomic
      |Rule of atomic*(atomic list);;

type program = clause list;;  

type prog_goal = atomic list;;

(* map applies f to each element of l *)
let rec map f l = match l with 
      |[] -> []
      |x::xs -> (f x)::(map f xs);;

let rec fold f e l = match l with 
      |[] -> e
      |x::xs -> fold f (f x e) xs;;

let rec filter f l = match l with
      |[] -> []
      |(x::xs) -> if (f x) then (x::(filter f xs)) else (filter f xs);;

let rec neg_filter f l = match l with
      |[] -> []
      |(x::xs) -> if (f x) then (filter f xs) else (x::(filter f xs));;
      
(* to check wether an element exists in a list *)
let rec find l e = match l with
      |[] -> false
      |x::xs -> if x = e then true else (find xs e);;  

(* to return union of two sets represented as list *)
let rec union l1 l2 = match l2 with
      |[] -> l1
      |x::xs -> if (find l1 x) then (union l1 xs) else (union (x::l1) xs);;

(* find if a pair with given first element exists *)
let rec find_pair pairlist v = match pairlist with 
      |[] -> false
      |(vx,t)::xs -> if (vx = v) then true else (find_pair xs v);;   

(* to return union based on keys of two list containing key value pair *)
let rec union_pair l1 l2 = match l2 with
      |[] -> l1
      |(v,t)::xs -> if (find_pair l1 v) then (union l1 xs) else (union ((v,t)::l1) xs);;

(* return substituent value for a variable *)
let rec findsub s v = match s with
      |[] -> v
      |(vx , t)::xs -> if (vx = v) then t else (findsub xs v);;   

(* implements homomorphic extension of substitution *)
let rec subst sigma t = match t with
      |Node(s,[]) -> (findsub sigma t)
      |Var v ->  (findsub sigma t)
      |Node(s,l) -> Node(s,(map (subst sigma) l))
      |_ -> t;; 

(* composes a particular substitution pair with another substitution *)
let rec give_sub sigma sub = match sub with
	|(t1,t2) -> (t1,subst sigma t2);;

(* composes two substitutions *)
let compose sig1 sig2 = union_pair (map (give_sub sig2) sig1) sig2;; 

(* implements fold operation on l1 l2 with a function which accepts two arguments *)
let rec unify s f l1 l2 = match (l1,l2) with
      |([],[]) -> s
      |(x1::xs1,x2::xs2) -> let s' = (compose s (f x1 x2)) in (unify s' f (map (subst s') xs1) (map (subst s') xs2))
      |([],(x::xs)) -> raise NOT_UNIFIABLE
      |((x::xs),[]) -> raise NOT_UNIFIABLE;;
      

let rec mgu t1 t2 = match (t1,t2) with
      |(Var v1, Const n2) -> [t1,t2]
      |(Const n1, Const n2) -> if (n1=n2) then [] else raise NOT_UNIFIABLE
      |(Const n1, Var v2) -> [t2,t1]
      |(Var v1, Nat n2) -> [t1,t2]
      |(Nat n1, Nat n2) -> if (n1=n2) then [] else raise NOT_UNIFIABLE
      |(Nat n1, Var v2) -> [t2,t1]
      |(Var v1, T) -> [t1,t2]
      |(T, Var v2) -> [t2,t1]
      |(T, T) -> []
      |(Var v1, F) -> [t1,t2]
      |(F, Var v2) -> [t2,t1]
      |(F, F) -> []
      |(Var v1, Var v2) -> if (v1=v2) then [] else [t1,t2]
      |(Var v1, Node(s,l)) -> if (find l t1) then raise NOT_UNIFIABLE else [t1,t2]
      |(Node(s,l), Var v2) -> if (find l t2) then raise NOT_UNIFIABLE else [t2,t1]
      |(Node(s1,l1),Node(s2,l2)) -> if (s1 = s2) then (unify [] mgu l1 l2) else raise NOT_UNIFIABLE
      |_ -> raise NOT_UNIFIABLE;;

let rec subst_atomic sigma atm = match atm with
      |(Pred(sym,l)) -> (Pred(sym,(map (subst sigma) l)))
      |_ -> atm;;

let rec mgu_atomic (Pred(sym1,l1)) (Pred(sym2,l2)) = 
      if (sym1 = sym2) 
      then (unify [] mgu l1 l2) 
      else raise NOT_UNIFIABLE;;

let rec find_vars t = match t with
      |Var v -> [t]
      |Node(s,(x::xs)) -> (fold union [] (map find_vars (x::xs)))
      |_ -> [];;

let find_vars_atomic (Pred(sym,l1)) = (fold union [] (map find_vars l1));;


let rec solve_goal prog workingprog goal = match (workingprog) with
      |[] -> ([],[])
      |((Fact atm1)::workingprog') -> 
            (try
                  let sol = mgu_atomic atm1 goal in
                  (sol,[workingprog'])
            with 
                  |NOT_UNIFIABLE -> solve_goal prog workingprog' goal)
      |((Rule (atm1,body))::workingprog') -> 
            try
                  let sol = mgu_atomic atm1 goal in
                  let newgoals = map (subst_atomic sol) body in
                  let newsols,workleft = solve_goallist prog newgoals in

                  ((union_pair newsols sol),(workleft@[workingprog]))
            with
                  |NOT_UNIFIABLE -> solve_goal prog workingprog' goal

and solve_goallist prog newgoals = match newgoals with
      |[] -> ([],[])
      |(x::xs) -> 
            let sol,left = (solve_goal prog prog x) in
            let newgoals = map (subst_atomic sol) xs in
            let newsols,workleft = solve_goallist prog newgoals in

            ((union_pair newsols sol),(workleft@left));;

let rec filter_unifier goalvars unif = match unif with
      |[] -> []
      |((v,sub)::xs) -> if (find goalvars v) then ((v,sub)::(filter_unifier goalvars xs)) else (filter_unifier goalvars xs);;

let rec print_terms t = match t with
      |Var v -> Printf.printf " Var %s " v
      |Const s-> Printf.printf " %s " s
      |Nat n -> Printf.printf " %d " n
      |T -> Printf.printf "true"
      |F -> Printf.printf "false"
      |Node (s,l) -> let _ = Printf.printf " Node %s " s in List.hd (map print_terms l);;

let rec print_unifs unifs = match unifs with
      |[] -> ()
      |((v,sub)::xs) -> let _ = print_terms v in
                        let _ = print_terms sub in
                        let _ = print_string "\n" in
                        print_unifs xs;;

let get1char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let top prog goal = 
      let goalvars = find_vars_atomic goal in
      let unifs,left = solve_goal prog prog goal in
      (print_unifs(filter_unifier goalvars unifs),left);;

      (*let choice = ref ';' in
      while (!choice = ';') do
            let unifs,left = solve_goal prog prog goal in
            print_unifs (filter_unifier goalvars unifs);
            choice := get1char()
      done;; *)

      
       
      


            




      
      
      