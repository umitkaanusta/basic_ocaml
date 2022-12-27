(* Named structures - records and tuples *)

(* 
  Records are composite of several types of data with named access,
  similar to structs in C.
*)
type pokemon_type = TNormal | TFire | TWater
(* Defining the 'pokemon' struct: *)
type pokemon = {name : string; hp : int; pokemon_type : pokemon_type;}
(* Note we can use pokemon_type both as a type and variable,
   namespaces for the two are separate in OCaml. *)

(* Instantiating *)
let charmander = {name = "Charmander"; hp = 39; pokemon_type = TFire}
let squirtle = {name = "Squirtle"; hp = 35; pokemon_type = TWater}
let bulbasaur = {name = "Bulbasaur"; hp = 45; pokemon_type = TNormal}
let _ = print_endline "\nGet name of Charmander:"
let _ = print_endline charmander.name

(* Basic pattern matching *)
let get_name p =
  match p with
  | {name = n; hp = _; pokemon_type = _} -> n
let _ = print_endline "Getting the names using pattern matching:"
let _ = get_name charmander |> print_endline
let _ = get_name squirtle |> print_endline
let _ = get_name bulbasaur |> print_endline

(* Basic pattern matching - syntactic sugar
   no need to separately assign pattern variable to each *)
let get_name_hp p =
  match p with
  | {name; hp; pokemon_type = _} -> name ^ string_of_int hp
let _ = print_endline "\nGet Name, HP of Squirtle:"
let _ = get_name_hp squirtle |> print_endline


(*
  Tuples are composites where values are identified by position. 
*)
let t = (1, 2, 5)
let sum_3tuple tup = 
  match tup with
  | (x, y, z) -> x + y + z

let _ = print_endline "\nSum of 3-tuple (1, 2, 5)"
let _ = sum_3tuple t |> string_of_int |> print_endline

(* Check CS3110 3.4 for the diff btwn Variants and these *)
