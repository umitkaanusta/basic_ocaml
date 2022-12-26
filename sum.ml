let rec sum lst =
  match lst with
  | [] -> 0
  | x :: xs -> x + sum xs