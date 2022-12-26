(* basics *)

let a = [1; 2; 3; 4]
let b = 1 :: 2 :: 3 :: []
let a_prepended = 5 :: 6 :: 7 :: a (* :: is the cons operator. O(1)*)
let a_appended = a @ [5; 6; 7] (* append works as concat'ing two lists. O(n)*)

let print_list lst =
    print_endline (String.concat " " (List.map string_of_int lst))

let _ = print_endline "\nPrinting several lists we created:"
let _ = print_list a
let _ = print_list b
let _ = print_list a_prepended
let _ = print_list a_appended

(* doing stuff over lists with pattern matching *)

(* SUMMING UP INT LIST *)

(* Summing up an integer list - head & tail idiom.
  1) Check if list has same shape with an empty list. If so return 0.
  2) Otherwise it has the same shape as the list h :: t (h is head, t is the rest).
    In that case, we return h + sum(t).
    It simply sums up starting from the left and accumulating.
*)
let rec list_sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + list_sum t

(* Summing up an int list - xs idiom 
  Logic is same.
  xs is the input list, x is the head, xs' is the rest.
*)
let rec list_sum_xs xs = 
  match xs with
  | [] -> 0
  | x :: xs' -> x + list_sum_xs xs'

let _ = print_endline "\nSum of a = [1, 2, 3, 4] with h :: t and x :: xs' idioms:"
let _ = list_sum a |> string_of_int |> print_endline
let _ = list_sum_xs a |> string_of_int |> print_endline


(* COMPUTE LEN OF LIST *)

(* Following h & t idiom, h ignored bcz we don't need it. *)
let rec list_len lst = 
  match lst with
  | [] -> 0
  | _ :: t -> 1 + list_len t

let _ = print_endline "\nLength of a = [1, 2, 3, 4]:"
let _ = list_len a |> string_of_int |> print_endline
let _ = print_endline "Length of a_appended = [1, 2, 3, 4, 5, 6, 7]:"
let _ = list_len a_appended |> string_of_int |> print_endline


(* IMPLEMENT APPEND FROM SCRATCH *)

(* Base idea: Append right to left = Prepend left to right *)
let rec list_append left_lst right_lst = 
  match left_lst with 
  | [] -> right_lst
  | h :: t -> h :: (list_append t right_lst)

let _ = print_endline "\nAppending [7; 8] to [1; 2]:"
let _ = list_append [1; 2] [7; 8] |> print_list
let _ = print_endline "Appending [9; 10] to a:"
let _ = list_append a [9; 10] |> print_list

(* -PRETENDING TO- MODIFY LISTS *)

(* Compiler does the following instead of copying the whole list 
   to save memory to enable top-down coding:
    Keep the unmodified part shared, change only the "changed" part.
    So "t" is shared btwn two lists, only h is changed below.
*)
let list_incr_first lst = 
  match lst with
  | [] -> []
  | h :: t -> h + 1 :: t

let _ = print_endline "\nIncr first element of [3; 2; 1]:"
let _ = list_incr_first [3; 2; 1] |> print_list

(* Let's increment all elements in the list *)
(* Given [3; 2; 1] - the steps:
    (i) [4; list_incr([2; 1])]
    (ii) [4; 3; list_incr([1])]
    (iii) [4; 3; 2]
    O(n) solution just like an imperative function.
*)
let rec list_incr lst = 
  match lst with
  | [] -> [] (* Empty list: don't do anything*)
  | h :: t -> h + 1 :: list_incr t (* List exists: Increment first, recurse the rest*)

let _ = print_endline "\nIncr all elements of [3; 2; 1]:"
let _ = list_incr [3; 2; 1] |> print_list

(* Tail recursion + Pattern matching *)
let rec list_sum_helper acc lst =
	match lst with
	| [] -> acc
	| x :: xs -> list_sum_helper (acc + x) xs
let list_sum_tr lst = list_sum_helper 0 lst

let _ = print_endline "\nSum up list [1; 2; 3; 4] with tail recursion:"
let _ = list_sum_tr a |> string_of_int |> print_endline


(* VARIANTS (Enum-like things) *)
(* Variant is a data type representing a value that is one of several possibilities. *)
(* Each "possibility" a constructor, there should be NO UNUSED CONSTRUCTORS. *)
type coin_face = Heads | Tails
let heads : coin_face = Heads (* val heads : coin_face = Heads *)
let tails : coin_face = Tails
(* Having defined all possibilities through variants, it perfectly enables pattern-matching *)
let int_of_coin_face face =
  match face with 
  | Heads -> 0
  | Tails -> 1
(* val int_of_day : day -> int <fun> *)
let _ = print_endline "\nConverting 'heads : coin_face' to int:"
let _ = int_of_coin_face heads |> string_of_int |> print_endline
let _ = print_endline "Converting 'tails : coin_face' to int:"
let _ = int_of_coin_face tails |> string_of_int |> print_endline
