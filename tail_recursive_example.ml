(* Implementing the factorial function in both ways *)

(* Normal way *)
let rec fact n = 
	if n = 0 then 1
	else n * fact (n - 1)


(* Tail-recursive way *)
(* the recursive func becomes a helper *)
let rec fact_aux n acc = (* acc argument added *)
	if n = 0 then acc (* base case returns acc *)
	else fact_aux (n - 1) (n * acc) (* change helper's recursive case *)

(* add a 'main' function calling the helper 
tr means tail recursive *)
let fact_tr n = 
	fact_aux n 1 (* original base case return value passed as acc *)


(* Testing both *)
let _ = print_endline (string_of_int (fact 5))
let _ = print_endline (string_of_int (fact_tr 5));