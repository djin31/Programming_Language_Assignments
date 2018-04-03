type 'a a_star = { seen: 'a list; unseen: 'a list; head: 'a ; tail: 'a};;

exception AtFirst;;

exception AtLast;;

exception TooShort;;

exception Empty;;

let rec listlength l = match l with 				(* listlength : 'a list -> int = <fun> *)
		[] -> 0 | x::xs -> 1 + (listlength xs);;

let lgh s = match s with							(* lgh : 'a a_star -> int = <fun> *)
		{seen=l1;unseen=l2} -> (List.length l1) + (List.length l2);;

let nonempty s = match s with						(* nonempty : 'a a_star -> bool = <fun> *)	
		{unseen=[]} -> false
		|{unseen=x::xs} -> true;;

let rec listreverse l = match l with				(* listreverse : 'a list -> 'a list = <fun> *)
		[]->[]|[a]->[a]|x::xs -> (listreverse xs)@[x];;

(* If one of the string is empty then we return the other string thus 
   lgh (concat s1 emptystring) = lgh (concat emptystring s1) = lgh (s1) = lgh (s1) + lgh empthystring
	
	lgh s = listlength s.seen + listlength s.unseen
	lgh (concat s1 s2)  = listlength (concat s1 s2).seen + listlength (concat s1 s2).unseen
						= listlength (s1.seen) + listlength (s1.unseen) + listlength (s2.seen) + listlength (s2.unseen)
						= (lgh s1) + (lgh s2) *)
let concat s1 s2 = match s1,s2 with					(* concat : 'a a_star -> 'a a_star -> 'a a_star = <fun> *)
		({unseen=[]},{unseen=[]}) -> s1
		|({unseen = x::xs},{unseen = []}) -> s1
		|({unseen = []},{unseen = x::xs}) -> s2
		|({unseen = x::xs},{unseen = y::ys}) -> {seen = s1.seen;unseen = s1.unseen @ (listreverse s2.seen) @ s2.unseen; head = s1.head;tail = s2.tail};;

let listtail l = match l with						(* listtail : 'a list -> 'a = <fun> *)
		[]->[]|[a]->[a]|x::xs -> xs;;

let listhead l = match l with						(* listhead : 'a list -> 'a = <fun> *)
		[]->raise AtLast|[a]->a|x::xs->x;;


(* lgh s = listlength s.seen + listlength s.unseen
	lgh (reverse s) = listlength (listtail s.unseen) + listlength ((listhead s.unseen) :: s.seen)
					= listlength s.unseen - 1 + 1 + listlength s.seen
					= lgh s *)
let reverse s = match s with						(* reverse : 'a a_star -> 'a a_star = <fun> *)
		{unseen=[]}->s
		|{unseen=x::xs}-> {seen= xs; unseen = x:: (s.seen); head=s.tail; tail= s.head};;


let first s = match s with							(* first : 'a a_star -> 'a = <fun> *)
			{unseen=[]}->raise Empty
			|{unseen = x::xs}-> s.head;;

let last s = match s with							(* last : 'a a_star -> 'a = <fun> *)
			{unseen=[]}->raise Empty
			|{unseen = x::xs}-> s.tail;;

let rec chararray i s l =							(* chararray : int -> string -> char list -> char list = <fun> *)
	if i < 0 then l else chararray (i - 1) s (s.[i] :: l);;

let rec listend l = match l with 					(* listend : 'a list -> 'a = <fun> *)
		[]->raise TooShort|[a]-> a|x::xs -> listend xs;;


let create s = 										(* create : string -> char a_star = <fun> *)
	let l = chararray (String.length s - 1) s [] in
	match l with
		[]-> {seen=[];unseen=[];head='a';tail='a'}
		|x::xs -> {seen=[];unseen=x::xs;head = x; tail = listend (x::xs) };;


let forward s = match s with						(* forward : 'a a_star -> 'a a_star = <fun> *) 	
		{unseen=[a]}-> raise AtLast
		|{unseen = []} -> raise AtLast
		|{seen = l1; unseen = x::y::xs} -> {seen=x::l1; unseen = y::xs; head = s.head; tail = s.tail};;

let back s = match s with							(* back : 'a a_star -> 'a a_star = <fun> *)
		{seen =[]} -> raise AtFirst
		|{seen = x::xs} -> {seen = xs; unseen = x::s.unseen ; head = s.head; tail = s.tail};;

let moveTo n s = 									(* moveTo : int -> 'a a_star -> 'a a_star = <fun> *)
	if n >= (lgh s) || n<0 then raise TooShort
	else
		let es = ref {seen=[];unseen=(listreverse s.seen) @ s.unseen;head=s.head;tail= s.tail} in
		for i = 0 to n-1 do
			es := forward !es
		done;
		!es;;


(* lgh (replace s w) = listlength s.seen + 1 + listlength (listtail s.unseen)
					 = listlength s.seen + 1 + listlength s.unseen -1
					 = lgh s
 *)
let replace s w = match s with						(* replace : 'a a_star -> 'a -> 'a a_star = <fun> *)
			{unseen=[]} -> raise AtLast
			|{seen=[];unseen=[x]} -> {seen=[];unseen = [w]; head = w; tail = w}
			|{seen=h1::t1;unseen=[x]} -> {seen = h1::t1; unseen = [w]; head= s.head; tail = w}
			|{seen= []; unseen = x::y::xs} -> {seen = []; unseen = w::y::xs; head = w; tail = s.tail} 
			|{seen = h1::t1; unseen = x::y::xs} -> {seen = h1::t1; unseen = w::y::xs; head = s.head ; tail = s.tail};;
