open Syntax

type exval =
    IntegerV of int
  | BooleanV of bool
  | FunctionV of id * exp * dnval Environment.t
  | RecFunV of  id * exp * dnval Environment.t ref
 and dnval = exval

let eval_binop op e1 e2 = match op , e1, e2 with
    Plus, IntegerV a, IntegerV b -> IntegerV (a+b)
  | Plus, _, _ ->  raise (Err "Eval Error: plus operator needs int ")
  | Minus, IntegerV a, IntegerV b -> IntegerV (a - b)
  | Minus, _, _ -> raise (Err "Eval Error: Minus operator needs int")
  | Times, IntegerV a, IntegerV b -> IntegerV (a * b)
  | Times, _, _ -> raise (Err "Eval Error: times operator needs int")
  | Div, IntegerV a, IntegerV b -> if b = 0 then raise (Err "Eval Error: divided by zero") else
                             IntegerV (a / b)
  | Div, _, _ -> raise (Err "Eval Error: div operator needs int")
  | And, BooleanV a, BooleanV b -> BooleanV (a && b)
  | And, _,_ -> raise (Err "Eval Error: && operator needs bool")
  | Or, BooleanV a, BooleanV b -> BooleanV (a || b)
  | Or, _, _ -> raise (Err "Eval Error: || operator needs bool")
  | Lt, IntegerV a, IntegerV b -> BooleanV (a < b)
  | Lt, _, _ -> raise (Err "Eval Error: < operator needs int")
  | Gt, IntegerV a, IntegerV b -> BooleanV (a > b)
  | Gt, _, _ -> raise (Err "Eval Error: > operator needs int")

let eval_singleop op exp = match op,exp with
    Not, BooleanV a -> BooleanV ( not a)
  | Not, IntegerV a -> if a = 0 then BooleanV (true) else BooleanV (false)

let rec eval_exp exp env = match exp with
    Var id ->( try Environment.lookup id env with Err e -> raise (Err ("Eval Error " ^e ^ " : " ^ id)))
  | IntV a -> IntegerV a
  | BoolV a -> BooleanV  a
  | BinOp (op, e1, e2) -> let arg1 = eval_exp e1 env in
                                       let arg2 = eval_exp e2 env in
                                       eval_binop op arg1 arg2
  | SingleOp (op, e) -> let arg1 = eval_exp e env in
                        eval_singleop op arg1
  | IfExp (e1, e2, e3) -> let arg1 = eval_exp e1 env in
                          (match arg1 with
                             BooleanV true -> eval_exp e2 env
                           | BooleanV false -> eval_exp e3 env
                           | _ -> raise (Err ("The expression must be boolean: if "))
                           )
  | LetExp (id, e1, e2) -> let arg1 = eval_exp e1 env in
                           eval_exp e2 ( Environment.extend id arg1 env)
  | LetAndExp (id, exp, andexp, exp2) -> let v = eval_exp exp env in
                                         let  (nid, nv, newenv2) = eval_anddecl andexp env in
                                         if id = nid then raise (Err "Eval Error: identifier is the same ") else
                                           eval_exp exp2 (Environment.extend id v newenv2)
  | FunExp (id, exp) -> FunctionV (id, exp, env)
   | AppExp (e1, e2) ->let funval = eval_exp e1 env in
                      let arg = eval_exp e2 env in
                      (
                        match funval with
                          FunctionV (id, body , env') ->
                          let newenv = Environment.extend id arg env' in
                          eval_exp body newenv
                        | RecFunV (id, exp, dummyenv )->
                           let newenv = Environment.extend id arg ( ! dummyenv) in
                           eval_exp exp newenv
                        | _ ->   raise (Err "Eval Error: Non-Function value is applied")
                      )
  | RecExp (id, para, e1, e2) -> let dummyenv = ref Environment.empty in
                                    let newenv =
                                      Environment.extend id (RecFunV (para, e1, dummyenv)) env in
                                    dummyenv := newenv;
                                    eval_exp e2 newenv

and  eval_anddecl exp env = match exp with
    SingleAndDecl (id, e) -> let v = eval_exp e  env in
                             let newenv = Environment.extend id v env in
                             (id, v, newenv)
  | CompAndDecl (id, e1,e2) -> let v = eval_exp e1 env in
                               let (nid, nv, newenv) =  eval_anddecl e2 env in
                                 (id, v, Environment.extend id v newenv)

let rec eval_decl  exp env = match exp with
    SingleDecl (id, e) -> let v = eval_exp e env in
                          (id, v, Environment.extend id v env)
  | CompDecl (id, e1, e2) -> (let v = eval_exp e1 env in
                             let newenv = Environment.extend id v env in
                             eval_decl e2 newenv)
  | AndDecl (id, e1, e2) ->( let v = eval_exp e1 env in
                             let (nid, nv , nenv)  =  eval_anddecl e2 env in
                             let newenv = (Environment.extend id v nenv) in
                             (id, v , newenv))

let eval_program pro env = match pro with
    Exp e -> let v =  eval_exp e env in (" - ", v , env)
  | Decl e  ->  eval_decl e env
