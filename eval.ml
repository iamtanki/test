open Syntax

exception EvalError
exception DividedZero

let eval_binop op e1 e2 = match op , e1, e2 with
    Plus, IntV a, IntV b -> IntV (a+b)
  | Plus, _, _ ->  raise EvalError
  | Minus, IntV a, IntV b -> IntV (a - b)
  | Minus, _, _ -> raise EvalError
  | Times, IntV a, IntV b -> IntV (a * b)
  | Times, _, _ -> raise EvalError
  | Div, IntV a, IntV b -> if b = 0 then raise DividedZero else
                             IntV (a / b)
  | Div, _, _ -> raise EvalError
  | And, BoolV a, BoolV b -> BoolV (a && b)
  | And, _,_ -> raise EvalError
  | Or, BoolV a, BoolV b -> BoolV (a || b)
  | Or, _, _ -> raise EvalError
  | Lt, IntV a, IntV b -> BoolV (a < b)
  | Lt, _, _ -> raise EvalError
  | Gt, IntV a, IntV b -> BoolV (a > b)
  | Gt, _, _ -> raise EvalError

let eval_singleop op exp = match op,exp with
    Not, BoolV a -> BoolV ( not a)
  | Not, IntV a -> if a = 0 then BoolV (true) else BoolV (false)

let rec eval_exp exp = match exp with
    IntV a -> IntV a
  | BoolV a -> BoolV a
  | BinOp (op, e1, e2) -> let arg1 = eval_exp e1 in
                                       let arg2 = eval_exp e2 in
                                       eval_binop op arg1 arg2
  | SingleOp (op, e) -> let arg1 = eval_exp e in
                        eval_singleop op arg1


let eval_program pro = match pro with
    Exp e -> eval_exp e
