#use "2016CS10312.ml";;

let alphabet=["1"; "2"; "a"; "b"; "c"; "A"];;

let nil = create "";;
let a = create "a";;
let abc = create "abc";;
let twelve = create "12";;

lgh nil;;
lgh a;;
lgh abc;;
lgh twelve;;


nonempty nil;;
nonempty a;;
nonempty twelve;;

concat nil nil;;
concat nil a;;
concat (create "1") nil;;
concat (create "1A") abc;;

reverse nil;;
reverse abc;;
reverse twelve;;

try first nil
with Empty -> '/';;
first a;;
first abc;;

try last nil
with Empty -> '/';;
last a;;
last abc;;

let editable = create "abac12a2aAac211";;

let edit1 = forward editable;;
let edit2 = back edit1;;
let edit3 = moveTo 10 edit2;;
replace edit3 'b';;

