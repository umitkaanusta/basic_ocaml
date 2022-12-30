(* TYPE SYNONYMS
 * New name for an existing type, building block for algebraic types *)
type point = float * float
type vector = float list
(* type matrix = float list list possible as well *)

let get_x = fun (x, _) -> x
let p1 : point = (1., 2.)
let _ = print_endline "\nGetting 'x' of point (1., 2.):"
let _ = get_x p1 |> string_of_float |> print_endline

(* Let's implement vector addition *)

let rec vec_add (v1 : vector) (v2 : vector) =
  (* Assume both have eq. length *)
  match v1 with
  | [] -> []
  | h1 :: t1 -> (match v2 with
    | [] -> []
    | h2 :: t2 -> h1 +. h2 :: vec_add t1 t2
    )

let rec vec_add_opt (v1 : vector) (v2 : vector) = 
  (* This can be done with 1 match only
   * Eq. length assumption stands.  *)
  match v1, v2 with
  | [], [] -> []
  | [], _ -> v2
  | _, [] -> v1
  | h1 :: t1, h2 :: t2 -> (h1 +. h2) :: (vec_add_opt t1 t2)

let vec_print lst =
  print_endline (String.concat " " (List.map string_of_float lst))

let vec1 : vector = [1.; 2.; 3.]
let vec2 : vector = [4.; 5.; 6.]
let _ = print_endline "\nAdding vectors [1.; 2.; 3.;] and [4.; 5.; 6.;]"
let _ = vec_print (vec_add vec1 vec2)
let _ = vec_print (vec_add_opt vec1 vec2)


(* OPTIONS
 * Offers 'maybe' type to cover possible Nones
 * without ever using null pointer and the risks associated
 * a.k.a the billion-dollar mistake by Sir Tony Hoare *)

(* A simple function *)
let get_val o = 
  match o with
  | None -> "null"
  | Some x -> string_of_int x

let _ = print_endline "\nPrinting the both possible states of an int option"
let notnull_int_option = Some 5
let null_int_option = None
let _ = get_val notnull_int_option |> print_endline
let _ = get_val null_int_option |> print_endline

(* Function computing max of a list 
 * that also covers the case when the list is empty *)
let rec list_max = function
  | [] -> None
  | h :: t -> begin
      (* With the second match, we open the box 't' *)
      match list_max t with
      (* If the rest is none, then head is the max (1 element) *)
      | None -> Some h
      (* If not, take the max of head & m, recur *)
      | Some m -> Some (max h m)
    end

let print_int_option = function
  | None -> print_endline "None";
  | Some (x : int) -> print_endline (string_of_int x)

let some_list = [2; 3; 99; 4; 5]
let empty_list = []
let _ = print_endline "\nUsing list_max on nonempty and empty lists:"
let _ = list_max some_list |> print_int_option
let _ = list_max empty_list |> print_int_option


(* ASSOCIATION LISTS 
 * A map (dictionary) implementation as a list of 2-tuples *)
let al_sides = [("rectangle", 4); ("nonagon", 9); ("pentagon", 5)]

(* Implementing insert (constant), lookup (linear) in association lists. 
 * More performance is possible through hash-map implementation. *)

let al_insert k v al =
  (k, v) :: al

let rec al_lookup key al =
  match al with
  | [] -> None
  | (k, v) :: t -> begin 
    (* Observe how options are used to cover non-existent key lookup *)
    if k = key then Some v else al_lookup key t
  end

let rec al_print al = 
  match al with
  | [] -> print_string ""
  | (k, v) :: rest -> begin
    Printf.printf "%s: %d " k v;
    al_print rest
  end

let _ = print_endline "\nPerforming insert and lookup operations on our AL"
let _ = print_endline "Initially:"
let _ = al_print al_sides
let _ = print_endline "\nInsert triangle: 3"
let al_sides = (al_insert "triangle" 3 al_sides)
let _ = al_print al_sides
let _ = print_endline "\nLookup rectangle:"
let sides_of_rectangle = (al_lookup "rectangle" al_sides)
let _ = print_int_option sides_of_rectangle
let _ = print_endline "Lookup some non-existent key:"
let weird_value = (al_lookup "tiger" al_sides)
let _ = print_int_option weird_value