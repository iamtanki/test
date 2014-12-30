open Syntax

type 'a t = (Syntax.id * 'a) list

let empty = []

let extend id v env = (id, v) :: env

let lookup id env =
  try
    List.assoc id env
  with
      _ -> raise (Err "Error: not found identifier from environment")

let rec map f = function
     [] -> []
  | (id, v) :: env -> (id, f v) :: map f env

let rec fold_right f  env a = match env with
     [] -> a
  | (_, v) :: tl -> f v (fold_right f tl a)
