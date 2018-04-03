type mystring  = {mutable seen: char list;mutable unseen: char list;mutable head: char ;mutable tail: char};;

exception AtFirst;;

exception AtLast;;

exception TooShort;;

let rec listlength l = match l with
		[] -> 0 | x::xs -> 1 + (listlength xs);;

let lgh s = match s with
		{seen=l1;unseen=l2} -> (List.length l1) + (List.length l2);;

let nonempty s = match s with	
		{unseen=[]} -> false
		|_ -> true;;

let rec listreverse l = match l with
		[]->[]|[a]->[a]|x::xs -> (listreverse xs)@[x];;

let concat s1 s2 = match s1,s2 with
		({unseen=[]},{unseen=[]}) -> s1
		|({unseen = x::xs},{unseen = []}) -> s1
		|({unseen = []},{unseen = x::xs}) -> s2
		|({unseen = x::xs},{unseen = y::ys}) -> {seen = s1.seen;unseen = s1.unseen @ (listreverse s2.seen) @ s2.unseen; head = s1.head;tail = s2.tail};;

let listtail l = match l with
		[]->[]|[a]->[a]|x::xs -> xs;;

let listhead l = match l with
		[]->raise AtLast|[a]->a|x::xs->x;;

let reverse s = match s with
		{unseen=[]}->s
		|{unseen=x::xs}-> {seen= listreverse xs; unseen = x:: (listreverse s.seen); head=s.tail; tail= s.head};;

let first s = s.head;;

let last s = s.tail;;


let rec chararray i s l =
	if i < 0 then l else chararray (i - 1) s (s.[i] :: l);;

let rec listend l = match l with 
		[]->raise TooShort|[a]-> a|x::xs -> listend xs;;


let create s = 
	let l = chararray (String.length s - 1) s [] in
	match l with
		[]-> {seen=[];unseen=[];head='a';tail='a'}
		|x::xs -> {seen=[];unseen=x::xs;head = x; tail = listend (x::xs) };;

let forward s = match s with 
		{unseen=[a]}-> raise AtLast
		|{unseen = []} -> raise AtLast
		|{seen = l1; unseen = x::y::xs} -> {seen=x::l1; unseen = y::xs; head = s.head; tail = s.tail};;

let back s = match s with
		{seen =[]} -> raise AtFirst
		|{seen = x::xs} -> {seen = xs; unseen = x::s.unseen ; head = s.head; tail = s.tail};;

let replace s w = match s with
			{unseen=[]} -> raise AtLast
			|{seen=[];unseen=[x]} -> {seen=[];unseen = [w]; head = w; tail = w}
			|{seen=h1::t1;unseen=[x]} -> {seen = h1::t1; unseen = [w]; head= s.head; tail = w}
			|{seen= []; unseen = x::y::xs} -> {seen = []; unseen = w::y::xs; head = w; tail = s.tail} 
			|{seen = h1::t1; unseen = x::y::xs} -> {seen = s.seen; unseen = w::y::xs; head = s.head ; tail = s.tail};;

let moveto n s = 
	if n >= (String.length s) || n<0 then raise TooShort
	else
		let es = ref (create s) in
		for i = 0 to n-1 do
			es := forward !es
		done;
		!es;;
