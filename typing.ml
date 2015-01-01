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

let rec  ty_exp exp tyenv = match exp with
    Var id -> (try Environment.lookup id tyenv  with Err e -> raise (Err (e ^ " : " ^ id)))
  | IntV _ -> TyInt
  | BoolV _ -> TyBool
  | BinOp (op, e1, e2) -> let arg1 = ty_exp e1 tyenv in
                          let arg2 = ty_exp e2 tyenv  in
                          ty_binop op arg1 arg2
  | SingleOp (op, e) -> let arg = ty_exp e tyenv in
                        ty_singleop op arg
  | IfExp (e1,e2,e3) -> let  arg1 = ty_exp e1 tyenv in
                        (
                          match arg1 with
                            TyBool -> let arg2 = ty_exp e2 tyenv in
                                       let arg3 = ty_exp e3 tyenv in
                                       if arg2 = arg3 then arg2
                                       else raise (Err " Error: then part, else part should have the same type")
                          | _ -> raise (Err "Error: if part should boolean type")
                        )
  | LetExp (id, e1, e2) -> let arg1 = ty_exp e1 tyenv in
                           ty_exp e2 (Environment.extend id arg1 tyenv)
  |LetAndExp (id, exp, andexp, exp2) -> let v = ty_exp exp tyenv in
                                        let (nid, nv, newtyenv) = ty_anddecl andexp tyenv in
                                        if id = nid then raise (Err "Error: identifier is the same ") else
                                        ty_exp exp2 newtyenv

and ty_anddecl exp tyenv  = match exp with
    SingleAndDecl (id, e) -> let v = ty_exp e tyenv in
                             (id, v , Environment.extend id  v tyenv)
  | CompAndDecl (id, e1,e2) -> let v = ty_exp e1 tyenv in
                               let (nid, nv, newtyenv) = ty_anddecl e2 tyenv in
                               (id,v, Environment.extend nid nv newtyenv)

let rec ty_decl exp tyenv = match exp with
    SingleDecl (id, e) -> let arg = ty_exp e tyenv in
                          (id, arg, Environment.extend id arg tyenv)
  | CompDecl (id, e1, e2) -> let arg = ty_exp e1 tyenv in
                             let newtyenv = Environment.extend id arg tyenv in
                             ty_decl e2 newtyenv
  | AndDecl (id, e1,e2) -> let v = ty_exp e1 tyenv in
                           let (nid, nv, newtyenv) = ty_anddecl e2 tyenv in
                           (id, v, Environment.extend nid nv newtyenv)

let ty_program pro tyenv =match pro with
    Exp e -> let v = ty_exp e tyenv in (" - ", v, tyenv)
  | Decl e -> let (id, v, newtyenv) = ty_decl e tyenv in (id,v,newtyenv)
