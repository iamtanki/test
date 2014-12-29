open Syntax

let ty_binop op t1 t2  = match op with
  | Plus | Minus | Times | Div -> (match t1, t2 with
              TyInt, TyInt -> TyInt
            | _ -> raise (Err "Error: needs integer type"))
  | And | Or -> (
                    match t1, t2 with
                      TyBool, TyBool -> TyBool
                    | _ -> raise (Err "Error: needs boolean type")
  )
  | Lt | Gt  -> (match t1 , t2 with
                   TyInt, TyInt -> TyBool
                 | _ -> raise (Err "Error: needs integer type"))

let ty_singleop op ty = match op with
    Not -> (match ty with
             TyInt -> TyBool
           | TyBool -> TyBool)

let rec  ty_exp exp = match exp with
    IntV _ -> TyInt
  | BoolV _ -> TyBool
  | BinOp (op, e1, e2) -> let arg1 = ty_exp e1 in
                          let arg2 = ty_exp e2 in
                          ty_binop op arg1 arg2
  | SingleOp (op, e) -> let arg = ty_exp e in
                        ty_singleop op arg
  | IfExp (e1,e2,e3) -> let  arg1 = ty_exp e1 in
                        (
                          match arg1 with
                            TyBool -> let arg2 = ty_exp e2 in
                                       let arg3 = ty_exp e3 in
                                       if arg2 = arg3 then arg2
                                       else raise (Err " Error: then part, else part should have the same type")
                          | _ -> raise (Err "Error: if part should boolean type")
                        )

let ty_program pro =match pro with
    Exp e -> ty_exp e
