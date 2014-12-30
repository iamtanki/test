open Syntax

let eval_binop op e1 e2 = match op , e1, e2 with
    Plus, IntV a, IntV b -> IntV (a+b)
  | Plus, _, _ ->  raise (Err "Error: plus operator needs int ")
  | Minus, IntV a, IntV b -> IntV (a - b)
  | Minus, _, _ -> raise (Err "Error: Minus operator needs int")
  | Times, IntV a, IntV b -> IntV (a * b)
  | Times, _, _ -> raise (Err "Error: times operator needs int")
  | Div, IntV a, IntV b -> if b = 0 then raise (Err "Error: divided by zero") else
                             IntV (a / b)
  | Div, _, _ -> raise (Err "Error: div operator needs int")
  | And, BoolV a, BoolV b -> BoolV (a && b)
  | And, _,_ -> raise (Err "Error: && operator needs bool")
  | Or, BoolV a, BoolV b -> BoolV (a || b)
  | Or, _, _ -> raise (Err "Error: || operator needs bool")
  | Lt, IntV a, IntV b -> BoolV (a < b)
  | Lt, _, _ -> raise (Err "Error: < operator needs int")
  | Gt, IntV a, IntV b -> BoolV (a > b)
  | Gt, _, _ -> raise (Err "Error: > operator needs int")

let eval_singleop op exp = match op,exp with
    Not, BoolV a -> BoolV ( not a)
  | Not, IntV a -> if a = 0 then BoolV (true) else BoolV (false)

let rec eval_exp exp env = match exp with
    Var id -> try Environment.lookup id env with Err e -> raise (Err (e ^ ": " ^ id))
  | IntV a -> IntV a
  | BoolV a -> BoolV a
  | BinOp (op, e1, e2) -> let arg1 = eval_exp e1 env in
                                       let arg2 = eval_exp e2 env in
                                       eval_binop op arg1 arg2
  | SingleOp (op, e) -> let arg1 = eval_exp e env in
                        eval_singleop op arg1
  | IfExp (e1, e2, e3) -> let arg1 = eval_exp e1 env in
                          (match arg1 with
                             BoolV true -> eval_exp e2 env
                           | BoolV false -> eval_exp e3 env
                           | _ -> raise (Err ("The expression must be boolean: if "))
                           )
  | LetExp (id, e1, e2) -> let arg1 = eval_exp e1 env in
                           eval_exp e2 ( Environment.extend id arg1)

let eval_program pro env = match pro with
    Exp e -> eval_exp e env
  | Decl (id, e) -> let v = eval_exp e in Environment.extend id v env; v
